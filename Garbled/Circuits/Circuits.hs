{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Garbled.Circuits.Circuits where

import Garbled.Circuits.Util

import qualified Data.Map as M
import           Control.Monad.State
import           Prelude hiding (or, and)
import qualified Data.Bits

type Map = M.Map

newtype Ref     = Ref     Int deriving (Enum, Ord, Eq, Show)
newtype InputId = InputId Int deriving (Enum, Ord, Eq, Show)

data Circuit = Input InputId
             | Const Bool
             | Not Ref
             | Xor Ref Ref
             | And Ref Ref
             | Or  Ref Ref
             deriving (Show, Eq, Ord)

circRefs :: Circuit -> [Ref]
circRefs (Not x   ) = [x]
circRefs (Xor x y ) = [x,y]
circRefs (And x y ) = [x,y]
circRefs (Or  x y ) = [x,y]
circRefs _          = []

data CircuitEnv = CircuitEnv { env_deref :: Map Ref Circuit
                             , env_dedup :: Map Circuit Ref 
                             } deriving (Show)

data CircuitSt = CircuitSt { st_nextRef     :: Ref
                           , st_inputs      :: [Ref]
                           , st_nextInputId :: InputId
                           , st_env         :: CircuitEnv
                           } deriving (Show)

type CircuitBuilder a = State CircuitSt a

data Program = Program { prog_inputs  :: [Ref]
                       , prog_outputs :: [Ref]
                       , prog_env     :: CircuitEnv
                       } deriving (Show)

buildCircuit :: CircuitBuilder [Ref] -> Program
buildCircuit c = Program { prog_inputs  = st_inputs st
                         , prog_outputs = outs
                         , prog_env     = st_env st 
                         }
  where
    (outs, st) = runState c emptySt
    emptySt = CircuitSt { st_nextRef     = Ref 0
                        , st_nextInputId = InputId 0
                        , st_inputs      = []
                        , st_env         = CircuitEnv M.empty M.empty
                        }

lookupCircuit :: Circuit -> CircuitBuilder (Maybe Ref)
lookupCircuit circ = do
  dedupEnv <- gets (env_dedup . st_env)
  return (M.lookup circ dedupEnv)

lookupRef :: Ref -> CircuitBuilder (Maybe Circuit)
lookupRef ref = do
  derefEnv <- gets (env_deref . st_env)
  return (M.lookup ref derefEnv)

insertRef :: Ref -> Circuit -> CircuitBuilder ()
insertRef ref circ = do
  derefEnv <- gets (env_deref . st_env)
  dedupEnv <- gets (env_dedup . st_env)
  modify (\st -> st { st_env = 
    CircuitEnv (M.insert ref circ derefEnv)
        (M.insert circ ref dedupEnv)
    })

nextRef :: CircuitBuilder Ref
nextRef = do
  ref <- gets st_nextRef
  modify (\st -> st { st_nextRef = succ ref })
  return ref

nextInputId :: CircuitBuilder InputId
nextInputId = do
  id <- gets st_nextInputId
  modify (\st -> st { st_nextInputId = succ id })
  return id

intern :: Circuit -> CircuitBuilder Ref
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

type EvalEnv = Map Ref Bool

eval :: Program -> [Bool] -> [Bool]
eval prog inps = evalState (mapM traverse (prog_outputs prog)) M.empty
  where
    env    = prog_env prog
    inputs = M.fromList (zip (map InputId [0..]) (reverse inps))
    
    traverse :: Ref -> State EvalEnv Bool
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

input :: CircuitBuilder Ref
input = do id  <- nextInputId
           ref <- intern (Input id)
           modify (\st -> st { st_inputs = st_inputs st ++ [ref] })
           return ref

xor :: Ref -> Ref -> CircuitBuilder Ref
xor x y = intern (Xor x y)

or :: Ref -> Ref -> CircuitBuilder Ref
or x y = intern (Or x y)

and :: Ref -> Ref -> CircuitBuilder Ref
and x y = intern (And x y)

not :: Ref -> CircuitBuilder Ref
not x = intern (Not x)

constant :: Bool -> CircuitBuilder Ref
constant b = intern (Const b)
