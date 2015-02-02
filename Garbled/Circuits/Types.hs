{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Garbled.Circuits.Types where

import           Control.Monad.State
import qualified Data.Map as M

type Map = M.Map

newtype CircRef  = CircRef  Int deriving (Enum, Ord, Eq, Show)
newtype InputId  = InputId  Int deriving (Enum, Ord, Eq, Show)

data Circ = Input InputId
          | Const Bool
          | Not CircRef
          | Xor CircRef CircRef
          | And CircRef CircRef
          | Or  CircRef CircRef
          deriving (Eq, Ord)

data Env c = Env { env_deref :: Map CircRef c
                 , env_dedup :: Map c CircRef
                 }

data Program c = Program { prog_inputs  :: [CircRef]
                         , prog_outputs :: [CircRef]
                         , prog_env     :: Env c
                         }

data TruthTable = TruthTable { tt_11 :: CircRef
                             , tt_10 :: CircRef
                             , tt_01 :: CircRef
                             , tt_00 :: CircRef 
                             } deriving (Eq, Ord)

type Secret    = Int
type Color     = Bool
type WireLabel = (Secret, Color)

data WireLabelPair = WireLabelPair { wl_true  :: WireLabel
                                   , wl_false :: WireLabel
                                   } deriving (Eq, Ord)

data GarbledGate = GarbledInput WireLabelPair
                 | GarbledGate { gate_inLeft  :: CircRef
                               , gate_inRight :: CircRef
                               , gate_table   :: TruthTable
                               } deriving (Eq, Ord)
