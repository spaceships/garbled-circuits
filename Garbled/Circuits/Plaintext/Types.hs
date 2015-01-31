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

type TruthTable = (CircRef, CircRef, CircRef, CircRef)

ttRefs :: TruthTable -> [CircRef]
ttRefs (a,b,c,d) = [a,b,c,d]
