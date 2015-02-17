{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Garbled.Circuits.Types where

import qualified Data.Map as M

type Map = M.Map

newtype Ref a = Ref Int deriving (Enum, Ord, Eq)

newtype InputId = InputId Int deriving (Enum, Ord, Eq)

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

type Color = Bool

data Wirelabel = Wirelabel { wl_col :: Color
                           , wl_val :: Int
                           } deriving (Show, Eq, Ord)

data WirelabelPair = WirelabelPair { wlp_true  :: Wirelabel
                                   , wlp_false :: Wirelabel
                                   } deriving (Show, Eq, Ord)

type GarbledGateTable = [((Color,Color), Wirelabel)]

data GarbledGate = GarbledInput InputId
                 | GarbledGate { gate_inpx  :: Ref GarbledGate
                               , gate_inpy  :: Ref GarbledGate
                               , gate_table :: GarbledGateTable
                               } deriving (Show, Eq, Ord)

class CanHaveChildren c where
  children :: c -> [Ref c]

instance CanHaveChildren Circ where
  children (Not x  ) = [x]
  children (Xor x y) = [x,y]
  children (And x y) = [x,y]
  children (Or  x y) = [x,y]
  children _         = []

instance CanHaveChildren TruthTable where
  children (TT {tt_inpx = x, tt_inpy = y}) = [x,y]
  children (TTInp _) = []

instance Eq TruthTable where
  TTInp a == TTInp b = a == b
  TTInp _ == TT {..} = False
  TT {..} == TTInp _ = False
  a == b = let init = tt_inpx a == tt_inpx b && tt_inpy a == tt_inpy b
               xs   = zipWith (==) (truthVals (tt_f a)) (truthVals (tt_f b))
           in foldl (&&) init xs

instance Ord TruthTable where
  TTInp a <= TTInp b = a <= b
  TTInp _ <= TT {..} = True
  TT {..} <= TTInp _ = False
  a <= b = let init = tt_inpx a <= tt_inpx b || tt_inpy a <= tt_inpy b
               xs   = zipWith (<=) (truthVals (tt_f a)) (truthVals (tt_f b))
           in foldl (||) init xs

instance Show TruthTable where
  show (TTInp id) = show id
  show (TT {tt_f = f}) = "TT" ++ map bit (truthVals f)
    where bit b = if b then '1' else '0'

instance CanHaveChildren GarbledGate where
  children (GarbledInput _) = []
  children g = [gate_inpx g, gate_inpy g]

instance Show (Ref c) where
  show (Ref x) = "<" ++ show x ++ ">"

instance Show InputId where
  show (InputId id) = "in" ++ show id

emptyProg :: Program c
emptyProg = Program { prog_inputs = [], prog_outputs = [], prog_env = emptyEnv }

emptyEnv :: Env c
emptyEnv = Env { env_deref = M.empty, env_dedup = M.empty }

-- teeny helpers
truthVals :: (Bool -> Bool -> Bool) -> [Bool]
truthVals f = [ f x y | x <- [True, False], y <- [True, False] ]
