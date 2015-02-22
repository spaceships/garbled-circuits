module Garbled.Circuits.Plaintext.Language
  ( CircBuilder
  , buildCirc
  , evalCirc
  , c_input
  , c_xor
  , c_and
  , c_not
  , c_or
  , c_const
  )
where

import Garbled.Circuits.Types
import Garbled.Circuits.Util (evalProg, err, lookupC)

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
  id <- gets st_nextInputId
  modify (\st -> st { st_nextInputId = succ id })
  return id

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

evalCirc :: Program Circ -> [Bool] -> IO [Bool]
evalCirc prog inps = evalProg reconstruct prog inps
  where
    inputs = M.fromList (zip (map InputId [0..]) inps)

    reconstruct :: Circ -> [Bool] -> IO Bool
    reconstruct (Input id) [] = case M.lookup id inputs of
      Just b  -> return b
      Nothing -> err "reconstruct" ("no input with id " ++ show id)
    reconstruct (Const x) []    = return x
    reconstruct (Not _)   [x]   = return $ Prelude.not x
    reconstruct (Xor _ _) [x,y] = return $ Data.Bits.xor x y
    reconstruct (And _ _) [x,y] = return $ x && y
    reconstruct (Or _ _)  [x,y] = return $ x || y
    reconstruct _ _ = err "reconstruct" "unrecognized pattern"

--------------------------------------------------------------------------------
-- smart constructors

c_input :: CircBuilder (Ref Circ)
c_input = do id  <- nextInputId
             ref <- intern (Input id)
             modify (\st -> st { st_inputs = S.insert ref (st_inputs st) })
             return ref

c_xor :: Ref Circ -> Ref Circ -> CircBuilder (Ref Circ)
c_xor x y = intern (Xor x y)

c_or :: Ref Circ -> Ref Circ -> CircBuilder (Ref Circ)
c_or x y = intern (Or x y)

c_and :: Ref Circ -> Ref Circ -> CircBuilder (Ref Circ)
c_and x y = intern (And x y)

c_not :: Ref Circ -> CircBuilder (Ref Circ)
c_not x = intern (Not x)

c_const :: Bool -> CircBuilder (Ref Circ)
c_const b = intern (Const b)
