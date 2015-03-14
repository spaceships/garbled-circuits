module Crypto.GarbledCircuits.Language
  ( CircBuilder
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
                     , st_inputs      :: S.Set (Ref Circ)
                     , st_nextInputId :: InputId
                     , st_env         :: Env Circ
                     }

type CircBuilder a = State CircSt a

buildCirc :: CircBuilder [Ref Circ] -> Program Circ
buildCirc c = Program { prog_inputs  = st_inputs st
                      , prog_outputs = outs
                      , prog_env     = st_env st
                      }
  where
    (outs, st) = runState c emptySt
    emptySt    = CircSt { st_nextRef     = Ref 0
                        , st_nextInputId = InputId 0
                        , st_inputs      = S.empty
                        , st_env         = emptyEnv
                        }

lookupCirc :: Circ -> CircBuilder (Maybe (Ref Circ))
lookupCirc circ = do
  dedupEnv <- gets (env_dedup . st_env)
  return (M.lookup circ dedupEnv)

lookupRef :: Ref Circ -> CircBuilder (Maybe Circ)
lookupRef ref = do
  derefEnv <- gets (env_deref . st_env)
  return (M.lookup ref derefEnv)

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

evalCirc :: [Bool] -> Program Circ -> [Bool]
evalCirc inps prog = evalProg reconstruct prog
  where
    inputs = M.fromList (zip (map InputId [0..]) inps)

    reconstruct :: Ref Circ -> Circ -> [Bool] -> Bool
    reconstruct _ (Input i) [] = case M.lookup i inputs of
      Just b  -> b
      Nothing -> err "reconstruct" ("no input with id " ++ show i)
    reconstruct _ (Const x) []    = x
    reconstruct _ (Not _)   [x]   = Prelude.not x
    reconstruct _ (Xor _ _) [x,y] = Data.Bits.xor x y
    reconstruct _ (And _ _) [x,y] = x && y
    reconstruct _ _ _ = err "reconstruct" "unrecognized pattern"

--------------------------------------------------------------------------------
-- smart constructors

c_input :: CircBuilder (Ref Circ)
c_input = do i   <- nextInputId
             ref <- intern (Input i)
             modify (\st -> st { st_inputs = S.insert ref (st_inputs st) })
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
c_or x y = do
    r0 <- c_not x
    r1 <- c_not y
    r2 <- c_and r0 r1
    c_not r2
