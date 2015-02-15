module Garbled.Circuits.Plaintext.Language where

import Garbled.Circuits.Types
import Garbled.Circuits.Util
import Garbled.Circuits.Plaintext.Rewrite

import           Control.Monad.State
import qualified Data.Bits
import qualified Data.Map as M
import           Prelude hiding (or, and)

data CircSt = CircSt { st_nextRef     :: Ref Circ
                     , st_inputs      :: [Ref Circ]
                     , st_nextInputId :: InputId
                     , st_env         :: Env Circ
                     }

type CircBuilder a = State CircSt a

buildCircuit :: CircBuilder [Ref Circ] -> Program Circ
buildCircuit c = Program { prog_inputs  = st_inputs st
                         , prog_outputs = outs
                         , prog_env     = st_env st
                         }
  where
    (outs, st) = runState c emptySt
    emptySt    = CircSt { st_nextRef     = Ref 0
                        , st_nextInputId = InputId 0
                        , st_inputs      = []
                        , st_env         = emptyEnv
                        }

lookupCircuit :: Circ -> CircBuilder (Maybe (Ref Circ))
lookupCircuit circ = do
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
  maybeRef <- lookupCircuit circ
  case maybeRef of
    Just ref -> return ref
    Nothing  -> do
      ref <- nextRef
      insertRef ref circ
      return ref

--------------------------------------------------------------------------------
-- plaintext evaluator

type EvalEnv = Map (Ref Circ) Bool

eval :: Program Circ -> [Bool] -> [Bool]
eval prog inps = reverse $ evalState (mapM traverse (prog_outputs prog)) M.empty
  where
    {-prog   = foldConsts p-}
    env    = prog_env prog
    inputs = M.fromList (zip (map InputId [0..]) inps)

    traverse :: Ref Circ -> State EvalEnv Bool
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

    reconstruct :: Circ -> [Bool] -> Bool
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

input :: CircBuilder (Ref Circ)
input = do id  <- nextInputId
           ref <- intern (Input id)
           modify (\st -> st { st_inputs = st_inputs st ++ [ref] })
           return ref

xor :: Ref Circ -> Ref Circ -> CircBuilder (Ref Circ)
xor x y = intern (Xor x y)

or :: Ref Circ -> Ref Circ -> CircBuilder (Ref Circ)
or x y = intern (Or x y)

and :: Ref Circ -> Ref Circ -> CircBuilder (Ref Circ)
and x y = intern (And x y)

not :: Ref Circ -> CircBuilder (Ref Circ)
not x = intern (Not x)

constant :: Bool -> CircBuilder (Ref Circ)
constant b = intern (Const b)
