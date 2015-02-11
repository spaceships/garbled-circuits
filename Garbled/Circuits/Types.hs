{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Garbled.Circuits.Types where

import           Control.Monad.State
import qualified Data.Map as M

type Map = M.Map

newtype Ref      = Ref      Int deriving (Enum, Ord, Eq, Show)
newtype InputId  = InputId  Int deriving (Enum, Ord, Eq, Show)

data Circ = Input InputId
          | Const Bool
          | Not Ref
          | Xor Ref Ref
          | And Ref Ref
          | Or  Ref Ref
          deriving (Eq, Ord)

data Env c = Env { env_deref :: Map Ref c
                 , env_dedup :: Map c Ref
                 }

data Program c = Program { prog_inputs  :: [Ref]
                         , prog_outputs :: [Ref]
                         , prog_env     :: Env c
                         }

data TruthTable = TruthTable { tt_11   :: Bool
                             , tt_10   :: Bool
                             , tt_01   :: Bool
                             , tt_00   :: Bool
                             , tt_inpx :: Ref
                             , tt_inpy :: Ref
                             , tt_out  :: Ref
                             } deriving (Eq, Ord)

type Secret    = Int
type Color     = Bool
type WireLabel = (Secret, Color)

data WireLabelPair = WireLabelPair { wl_true  :: WireLabel
                                   , wl_false :: WireLabel
                                   } deriving (Eq, Ord)

data GarbledGate = GarbledInput WireLabelPair
                 | GarbledGate { gate_inLeft  :: Ref
                               , gate_inRight :: Ref
                               , gate_table   :: TruthTable
                               } deriving (Eq, Ord)
