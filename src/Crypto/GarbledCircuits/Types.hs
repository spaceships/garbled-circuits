{-# LANGUAGE PackageImports, GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Crypto.GarbledCircuits.Types where

import           Control.Monad.Reader
import           Control.Monad.State (StateT, State)
import           Crypto.Cipher.AES128
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Word
import           Numeric (showHex)
import           System.Entropy

type Map = M.Map
type Set = S.Set
type ByteString = BS.ByteString

newtype Ref a = Ref { unRef :: Word16 } deriving (Enum, Ord, Eq)

data Party = PartyA | PartyB deriving (Enum, Ord, Eq, Show)

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

data Program c = Program { prog_input_a :: Set (Ref c)
                         , prog_input_b :: Set (Ref c)
                         , prog_output  :: [Ref c]
                         , prog_env     :: Env c
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
prog_inputs PartyA = prog_input_a
prog_inputs PartyB = prog_input_b

lsb :: Wirelabel -> Bool
lsb wl = BS.last wl .&. 1 > 0

zeroWirelabel :: Wirelabel
zeroWirelabel = BS.replicate 16 0

showWirelabel :: Wirelabel -> String
showWirelabel wl = "wl" ++ showCol (lsb wl) ++ " " ++ hexStr
    where showCol b = if b then "1" else "0"
          hexStr = concatMap (pad . hex) $ BS.unpack wl
          pad s = if length s == 1 then '0' : s else s
          hex = flip showHex ""

emptyProg :: Program c
emptyProg = Program { prog_input_a = S.empty
                    , prog_input_b = S.empty
                    , prog_output  = []
                    , prog_env     = emptyEnv
                    }

emptyEnv :: Env c
emptyEnv = M.empty

truthVals :: (Bool -> Bool -> Bool) -> [Bool]
truthVals f = [ f x y | x <- [True, False], y <- [True, False] ]

bitc :: Bool -> Char
bitc b = if b then '1' else '0'

emptyContext :: Context
emptyContext = Context M.empty M.empty undefined undefined 0

wlp_true :: WirelabelPair -> Wirelabel
wlp_true = snd

wlp_false :: WirelabelPair -> Wirelabel
wlp_false = fst

circ2op :: Circuit -> Operation
circ2op (Input _ _) = INPUT
circ2op (Const   _) = CONST
circ2op (Not     _) = NOT
circ2op (Xor   _ _) = XOR
circ2op (And   _ _) = AND
circ2op (Or    _ _) = OR

emptyTT :: TruthTable
emptyTT = TT { tt_f    = undefined
             , tt_inpx = undefined
             , tt_inpy = undefined
             , tt_negx = False
             , tt_negy = False
             }
