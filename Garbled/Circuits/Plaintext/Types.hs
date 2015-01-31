{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Garbled.Circuits.Plaintext.Types where

import qualified Data.Map as M
import           Control.Monad.State

type Map = M.Map

newtype CircRef  = CircRef  Int deriving (Enum, Ord, Eq, Show)
newtype InputId  = InputId  Int deriving (Enum, Ord, Eq, Show)
newtype OutputId = OutputId Int deriving (Enum, Ord, Eq, Show)

data Circuit = Input InputId
             | Const Bool
             | Not CircRef
             | Xor CircRef CircRef
             | And CircRef CircRef
             | Or  CircRef CircRef
             deriving (Show, Eq, Ord)

circRefs :: Circuit -> [CircRef]
circRefs (Not x  ) = [x]
circRefs (Xor x y) = [x,y]
circRefs (And x y) = [x,y]
circRefs (Or  x y) = [x,y]
circRefs _         = []

data Env c = Env { env_deref :: Map CircRef c
                 , env_dedup :: Map c CircRef
                 }

data CircuitSt = CircuitSt { st_nextRef     :: CircRef
                           , st_inputs      :: [CircRef]
                           , st_nextInputId :: InputId
                           , st_env         :: Env Circuit
                           }

type CircuitBuilder a = State CircuitSt a

data Program c = Program { prog_inputs  :: [CircRef]
                         , prog_outputs :: [CircRef]
                         , prog_env     :: Env c
                         }

newtype TruthTable = TruthTable (CircRef, CircRef, CircRef, CircRef) 
                   deriving (Ord, Eq)

ttRefs :: TruthTable -> [CircRef]
ttRefs (TruthTable (a,b,c,d)) = [a,b,c,d]

emptyProgram :: Program c
emptyProgram = Program { prog_inputs = [], prog_outputs = [], prog_env = emptyEnv }

emptyEnv :: Env c
emptyEnv = Env { env_deref = M.empty, env_dedup = M.empty }
