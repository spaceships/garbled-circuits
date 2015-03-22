{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Crypto.GarbledCircuits.Types where

import           Control.Monad.Reader
import           Control.Monad.State (StateT)
import           Crypto.Cipher.AES128
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Word

type Map = M.Map
type Set = S.Set
type ByteString = BS.ByteString

newtype Ref a = Ref { unRef :: Int } deriving (Enum, Ord, Eq)

data Party = Garbler | Evaluator deriving (Enum, Ord, Eq, Show)

newtype InputId = InputId { getInputId :: Int } deriving (Enum, Ord, Eq)

data Circuit = Input InputId Party
             | Const Bool
             | Not (Ref Circuit)
             | Xor (Ref Circuit) (Ref Circuit)
             | And (Ref Circuit) (Ref Circuit)
             | Or  (Ref Circuit) (Ref Circuit)
             deriving (Eq, Ord, Show)

-- it is convenient to have a Circuit without associated data
data Operation = INPUT
               | CONST
               | NOT
               | XOR
               | AND
               | OR
               deriving (Show, Eq, Ord)

type Env c = Map (Ref c) c

data Program c = Program { prog_input_gb :: Set (Ref c)
                         , prog_input_ev :: Set (Ref c)
                         , prog_output   :: [Ref c]
                         , prog_env      :: Env c
                         } deriving (Show, Eq)

data TruthTable = TTInp InputId Party
                | TT { tt_f    :: Operation
                     , tt_inpx :: Ref TruthTable
                     , tt_inpy :: Ref TruthTable
                     , tt_negx :: Bool
                     , tt_negy :: Bool
                     } deriving (Ord, Eq, Show)

--------------------------------------------------------------------------------
-- data types for garbling

type Wirelabel = BS.ByteString

type Key = BS.ByteString

type WirelabelPair = (Wirelabel, Wirelabel)

data GarbledGate = GarbledInput InputId Party
                 | FreeXor  (Ref GarbledGate) (Ref GarbledGate)
                 | HalfGate (Ref GarbledGate) (Ref GarbledGate) Wirelabel Wirelabel
                 deriving (Show, Eq, Ord)

type Garble = StateT (Program GarbledGate) 
                (ReaderT (Program TruthTable) 
                  (StateT Context
                    IO))

data Context = Context { ctx_pairs :: Map (Ref GarbledGate) WirelabelPair
                       , ctx_truth :: Map Wirelabel Bool
                       , ctx_key   :: AESKey128
                       , ctx_r     :: Wirelabel
                       , ctx_ctr   :: Int
                       }

--------------------------------------------------------------------------------
-- instances

class CanHaveChildren c where
  children :: c -> [Ref c]

instance CanHaveChildren Circuit where
  children (Not x  ) = [x]
  children (Xor x y) = [x,y]
  children (And x y) = [x,y]
  children (Or  x y) = [x,y]
  children _         = []

instance CanHaveChildren TruthTable where
  children (TT {tt_inpx = x, tt_inpy = y}) = [x,y]
  children (TTInp _ _) = []

instance CanHaveChildren GarbledGate where
  children (GarbledInput _ _) = []
  children (FreeXor      x y) = [x,y]
  children (HalfGate x y _ _) = [x,y]

instance Show (Ref c) where
  show (Ref x) = "<" ++ show x ++ ">"

instance Show InputId where
  show (InputId i) = "in" ++ show i

--------------------------------------------------------------------------------
-- helpers

prog_inputs :: Party -> Program c -> Set (Ref c)
prog_inputs Garbler   = prog_input_gb
prog_inputs Evaluator = prog_input_ev

zeroWirelabel :: Wirelabel
zeroWirelabel = BS.replicate 16 0

emptyProg :: Program c
emptyProg = Program { prog_input_gb = S.empty
                    , prog_input_ev = S.empty
                    , prog_output   = []
                    , prog_env      = emptyEnv
                    }

emptyEnv :: Env c
emptyEnv = M.empty

emptyContext :: Context
emptyContext = Context M.empty M.empty undefined undefined 0

wlp_true :: WirelabelPair -> Wirelabel
wlp_true = snd

wlp_false :: WirelabelPair -> Wirelabel
wlp_false = fst

emptyTT :: TruthTable
emptyTT = TT { tt_f    = undefined
             , tt_inpx = undefined
             , tt_inpy = undefined
             , tt_negx = False
             , tt_negy = False
             }
