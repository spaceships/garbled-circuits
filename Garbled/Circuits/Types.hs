{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, FlexibleInstances #-}

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
          deriving (Eq, Ord, Show)

data Env c = Env { env_deref :: Map (Ref c) c
                 , env_dedup :: Map c (Ref c)
                 } deriving (Show)

data Program c = Program { prog_inputs  :: [Ref c]
                         , prog_outputs :: [Ref c]
                         , prog_env     :: Env c
                         } deriving (Show)

data TruthTable = TTInp InputId
                | TT { tt_f    :: Bool -> Bool -> Bool 
                     , tt_inpx :: Ref TruthTable
                     , tt_inpy :: Ref TruthTable
                     }

instance Eq TruthTable where
  TTInp a == TTInp b = a == b
  TT {tt_inpx = ax, tt_inpy = ay, tt_f = fa} == TT {tt_inpx = bx, tt_inpy = by, tt_f = fb} = 
    ax == bx && ay == by && 
    fa True  True  == fb True  True  &&
    fa True  False == fb True  False &&
    fa False True  == fb False True  &&
    fa False False == fb False False
  _ == _ = False

instance Ord TruthTable where
  TTInp a <= TTInp b = a <= b
  TT {tt_inpx = ax, tt_inpy = ay, tt_f = fa} <= TT {tt_inpx = bx, tt_inpy = by, tt_f = fb} = 
    ax <= bx || ay <= by || 
    fa True  True  <= fb True  True  ||
    fa True  False <= fb True  False ||
    fa False True  <= fb False True  ||
    fa False False <= fb False False
  TTInp _ <= TT {..} = True
  TT {..} <= TTInp _ = False

instance Show TruthTable where
  show (TT {tt_f = f}) = "TT" ++ bit (f True  True) ++ bit (f True  False) 
                              ++ bit (f False True) ++ bit (f False False)
    where bit b = if b then "1" else "0"
  show (TTInp id) = "TTInp " ++ show id

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

circ2op :: Circ -> Operation
circ2op (Input _) = OInput
circ2op (Const _) = OConst
circ2op (Not   _) = ONot
circ2op (Xor _ _) = OXor
circ2op (And _ _) = OAnd
circ2op (Or  _ _) = OOr

{-data BinaryOp = -}

data Operation = OInput
               | OConst
               | ONot
               | OXor
               | OAnd
               | OOr
               deriving (Show, Eq, Ord)
