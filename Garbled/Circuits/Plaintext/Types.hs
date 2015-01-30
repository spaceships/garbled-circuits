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
circRefs (Not x   ) = [x]
circRefs (Xor x y ) = [x,y]
circRefs (And x y ) = [x,y]
circRefs (Or  x y ) = [x,y]
circRefs _          = []

data CircuitEnv = CircuitEnv { env_deref :: Map CircRef Circuit
                             , env_dedup :: Map Circuit CircRef
                             } deriving (Show)

data CircuitSt = CircuitSt { st_nextRef     :: CircRef
                           , st_inputs      :: [CircRef]
                           , st_nextInputId :: InputId
                           , st_env         :: CircuitEnv
                           } deriving (Show)

type CircuitBuilder a = State CircuitSt a

data Program = Program { prog_inputs  :: [CircRef]
                       , prog_outputs :: [CircRef]
                       , prog_env     :: CircuitEnv
                       } deriving (Show)

