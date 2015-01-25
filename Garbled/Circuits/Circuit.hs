{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Garbled.Circuits.Circuit where

import qualified Data.Map as M
import           Control.Monad.State
import           Prelude hiding (or, and)
import qualified Data.Bits

type Map = M.Map

-- circuit language
-- garbled circuit representation
-- garbling
-- ot
-- evaluation

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

data Env = Env { env_deref :: Map Ref Circuit
               , env_dedup :: Map Circuit Ref 
               } deriving (Show)

data CircuitSt = CircuitSt { st_nextRef     :: Ref
                     , st_nextInputId :: InputId
                     , st_env         :: Env
                     } deriving (Show)

type CircuitBuilder a = State CircuitSt a

buildCircuit :: CircuitBuilder a -> (a, Env)
buildCircuit c = let (out, st) = runState c emptySt in (out, st_env st)
  where
    emptySt = CircuitSt { st_nextRef     = Ref 0
                        , st_nextInputId = InputId 0
                        , st_env         = Env M.empty M.empty
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
    Env (M.insert ref circ derefEnv)
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

eval :: CircuitBuilder [Ref] -> [Bool] -> [Bool]
eval circ inps = evalState (mapM traverse outRefs) M.empty
  where
    (outRefs, env) = buildCircuit circ
    inputs         = M.fromList (zip (map InputId [0..]) (reverse inps))
    
    traverse :: Ref -> State EvalEnv Bool
    traverse ref = do
      precomputed <- get
      case M.lookup ref precomputed of
        Just b  -> return b
        Nothing -> do
          let circ = violentLookup ref (env_deref env)
          children <- mapM traverse (circRefs circ)
          let result = reconstruct circ children
          put (M.insert ref result precomputed)
          return result

    violentLookup r e = case M.lookup r e of
      Nothing -> error "[violentLookup] something went horribly wrong"
      Just x  -> x

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
input = do id <- nextInputId
           intern (Input id)

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
