module Garbled.Circuits.Plaintext.Language where

import Garbled.Circuits.Util
import Garbled.Circuits.Plaintext.Types
import Garbled.Circuits.Plaintext.Rewrite

import qualified Data.Map as M
import           Control.Monad.State
import           Prelude hiding (or, and)
import qualified Data.Bits

buildCircuit :: CircuitBuilder [CircRef] -> Program
buildCircuit c = Program { prog_inputs  = st_inputs st
                         , prog_outputs = outs
                         , prog_env     = st_env st 
                         }
  where
    (outs, st) = runState c emptySt
    emptySt = CircuitSt { st_nextRef     = CircRef 0
                        , st_nextInputId = InputId 0
                        , st_inputs      = []
                        , st_env         = CircuitEnv M.empty M.empty
                        }

lookupCircuit :: Circuit -> CircuitBuilder (Maybe CircRef)
lookupCircuit circ = do
  dedupEnv <- gets (env_dedup . st_env)
  return (M.lookup circ dedupEnv)

lookupRef :: CircRef -> CircuitBuilder (Maybe Circuit)
lookupRef ref = do
  derefEnv <- gets (env_deref . st_env)
  return (M.lookup ref derefEnv)

insertRef :: CircRef -> Circuit -> CircuitBuilder ()
insertRef ref circ = do
  derefEnv <- gets (env_deref . st_env)
  dedupEnv <- gets (env_dedup . st_env)
  modify (\st -> st { st_env = 
    CircuitEnv (M.insert ref circ derefEnv)
        (M.insert circ ref dedupEnv)
    })

nextRef :: CircuitBuilder CircRef
nextRef = do
  ref <- gets st_nextRef
  modify (\st -> st { st_nextRef = succ ref })
  return ref

nextInputId :: CircuitBuilder InputId
nextInputId = do
  id <- gets st_nextInputId
  modify (\st -> st { st_nextInputId = succ id })
  return id

intern :: Circuit -> CircuitBuilder CircRef
intern circ = do
  maybeRef <- lookupCircuit circ
  case maybeRef of
    Just ref -> return ref
    Nothing  -> do
      ref <- nextRef
      insertRef ref circ
      return ref

--------------------------------------------------------------------------------
-- plaintext evaluator

type EvalEnv = Map CircRef Bool

eval :: Program -> [Bool] -> [Bool]
eval p inps = evalState (mapM traverse (prog_outputs prog)) M.empty
  where
    prog   = foldConsts p
    env    = prog_env prog
    inputs = M.fromList (zip (map InputId [0..]) (reverse inps))
    
    traverse :: CircRef -> State EvalEnv Bool
    traverse ref = do
      precomputed <- get
      case M.lookup ref precomputed of
        Just b  -> return b
        Nothing -> do
          let circ = violentLookup ref (env_deref env)
          children <- mapM traverse (circRefs circ)
          let result = reconstruct circ children
          modify (M.insert ref result)
          return result

    reconstruct :: Circuit -> [Bool] -> Bool
    reconstruct (Input id) [] = case M.lookup id inputs of
      Just b  -> b
      Nothing -> error $ "[reconstruct] No input with id " ++ show id
    reconstruct (Const x) []    = x
    reconstruct (Not _)   [x]   = Prelude.not x
    reconstruct (Xor _ _) [x,y] = Data.Bits.xor x y
    reconstruct (And _ _) [x,y] = x && y
    reconstruct (Or _ _)  [x,y] = x || y

--------------------------------------------------------------------------------
-- smart constructors

input :: CircuitBuilder CircRef
input = do id  <- nextInputId
           ref <- intern (Input id)
           modify (\st -> st { st_inputs = st_inputs st ++ [ref] })
           return ref

xor :: CircRef -> CircRef -> CircuitBuilder CircRef
xor x y = intern (Xor x y)

or :: CircRef -> CircRef -> CircuitBuilder CircRef
or x y = intern (Or x y)

and :: CircRef -> CircRef -> CircuitBuilder CircRef
and x y = intern (And x y)

not :: CircRef -> CircuitBuilder CircRef
not x = intern (Not x)

constant :: Bool -> CircuitBuilder CircRef
constant b = intern (Const b)
