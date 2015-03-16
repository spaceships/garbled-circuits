module Crypto.GarbledCircuits.Language
  ( 
    CircBuilder
  , buildCirc
  , evalCirc
  , intern
  , c_input
  , c_xor
  , c_and
  , c_not
  , c_or
  , c_const
  )
where

import Crypto.GarbledCircuits.Types
import Crypto.GarbledCircuits.Util hiding (nextRef)

import           Control.Monad.State
import qualified Data.Bits
import qualified Data.Map as M
import qualified Data.Set as S

data CircSt = CircSt { st_nextRef     :: Ref Circ
                     , st_inputs_a    :: S.Set (Ref Circ)
                     , st_inputs_b    :: S.Set (Ref Circ)
                     , st_nextInputId :: InputId
                     , st_env         :: Env Circ
                     }

type CircBuilder a = State CircSt a

buildCirc :: CircBuilder [Ref Circ] -> Program Circ
buildCirc c = Program { prog_inputs_a = st_inputs_a st
                      , prog_inputs_b = st_inputs_b st
                      , prog_outputs  = reverse outs
                      , prog_env      = st_env st
                      }
  where
    (outs, st) = runState c emptySt
    emptySt    = CircSt { st_nextRef     = Ref 0
                        , st_nextInputId = InputId 0
                        , st_inputs_a    = S.empty
                        , st_inputs_b    = S.empty
                        , st_env         = emptyEnv
                        }

lookupCirc :: Circ -> CircBuilder (Maybe (Ref Circ))
lookupCirc circ = do
  dedupEnv <- gets (env_dedup . st_env)
  return (M.lookup circ dedupEnv)

insertRef :: Ref Circ -> Circ -> CircBuilder ()
insertRef ref circ = do
  derefEnv <- gets (env_deref . st_env)
  dedupEnv <- gets (env_dedup . st_env)
  modify (\st -> st { st_env =
    Env (M.insert ref circ derefEnv)
        (M.insert circ ref dedupEnv)
    })

nextRef :: CircBuilder (Ref Circ)
nextRef = do
  ref <- gets st_nextRef
  modify (\st -> st { st_nextRef = succ ref })
  return ref

nextInputId :: CircBuilder InputId
nextInputId = do
  i <- gets st_nextInputId
  modify (\st -> st { st_nextInputId = succ i })
  return i

intern :: Circ -> CircBuilder (Ref Circ)
intern circ = do
  maybeRef <- lookupCirc circ
  case maybeRef of
    Just ref -> return ref
    Nothing  -> do
      ref <- nextRef
      insertRef ref circ
      return ref

--------------------------------------------------------------------------------
-- plaintext evaluator

evalCirc :: [Bool] -> [Bool] -> Program Circ -> [Bool]
evalCirc inpA inpB prog = evalProg construct prog
  where
    inputs A = M.fromList (zip (S.toList (prog_inputs_a prog)) inpA)
    inputs B = M.fromList (zip (S.toList (prog_inputs_b prog)) inpB)

    construct :: Ref Circ -> Circ -> [Bool] -> Bool
    construct ref (Input i p) [] = case M.lookup ref (inputs p) of
      Just b  -> b
      Nothing -> err "reconstruct" ("no input with id " ++ show i)
    construct _ (Const x) []    = x
    construct _ (Not _)   [x]   = Prelude.not x
    construct _ (Xor _ _) [x,y] = Data.Bits.xor x y
    construct _ (And _ _) [x,y] = x && y
    construct _ (Or  _ _) [x,y] = x || y
    construct _ _ _ = err "reconstruct" "unrecognized pattern"

--------------------------------------------------------------------------------
-- smart constructors

c_input :: Party -> CircBuilder (Ref Circ)
c_input p = do i   <- nextInputId
               ref <- intern (Input i p)
               modify $ \st -> case p of
                 A -> st { st_inputs_a = S.insert ref (st_inputs_a st) }
                 B -> st { st_inputs_b = S.insert ref (st_inputs_b st) }
               return ref

c_xor :: Ref Circ -> Ref Circ -> CircBuilder (Ref Circ)
c_xor x y = intern (Xor x y)

c_and :: Ref Circ -> Ref Circ -> CircBuilder (Ref Circ)
c_and x y = intern (And x y)

c_not :: Ref Circ -> CircBuilder (Ref Circ)
c_not x = intern (Not x)

c_const :: Bool -> CircBuilder (Ref Circ)
c_const b = intern (Const b)

c_or :: Ref Circ -> Ref Circ -> CircBuilder (Ref Circ)
c_or x y = intern (Or x y)
