{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Garbled.Circuits.Types where

import           Control.Monad.State
import qualified Data.Map as M

type Map = M.Map

newtype Ref a    = Ref      Int deriving (Enum, Ord, Eq, Show)
newtype InputId  = InputId  Int deriving (Enum, Ord, Eq, Show)

data Circ = Input InputId
          | Const Bool
          | Not (Ref Circ)
          | Xor (Ref Circ) (Ref Circ)
          | And (Ref Circ) (Ref Circ)
          | Or  (Ref Circ) (Ref Circ)
          deriving (Eq, Ord)

data Env c = Env { env_deref :: Map (Ref c) c
                 , env_dedup :: Map c (Ref c)
                 }

data Program c = Program { prog_inputs  :: [Ref c]
                         , prog_outputs :: [Ref c]
                         , prog_env     :: Env c
                         }

data TruthTable = TruthTable { tt_11   :: Bool
                             , tt_10   :: Bool
                             , tt_01   :: Bool
                             , tt_00   :: Bool
                             , tt_inpx :: Ref TruthTable
                             , tt_inpy :: Ref TruthTable
                             } deriving (Eq, Ord)

type Secret    = Int
type Color     = Bool
type WireLabel = (Secret, Color)

data WireLabelPair = WireLabelPair { wl_true  :: WireLabel
                                   , wl_false :: WireLabel
                                   } deriving (Eq, Ord)

data GarbledGate = GarbledInput WireLabelPair
                 | GarbledGate { gate_inLeft  :: Ref GarbledGate
                               , gate_inRight :: Ref GarbledGate
                               , gate_table   :: TruthTable
                               } deriving (Eq, Ord)


class CanHaveChildren c where
  children :: c -> [Ref c]

instance CanHaveChildren Circ where
  children = circRefs

instance CanHaveChildren TruthTable where
  children = ttRefs

circRefs :: Circ -> [Ref Circ]
circRefs (Not x  ) = [x]
circRefs (Xor x y) = [x,y]
circRefs (And x y) = [x,y]
circRefs (Or  x y) = [x,y]
circRefs _         = []

ttRefs :: TruthTable -> [Ref TruthTable]
ttRefs tt = [tt_inpx tt, tt_inpy tt]

