{-# LANGUAGE PackageImports, RecordWildCards, GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Crypto.GarbledCircuits.Types where

import           Control.Monad.Reader
import           Control.Monad.State
import           Crypto.Cipher.AES
import           "crypto-random" Crypto.Random
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Word
import           Numeric (showHex)

type Map = M.Map
type Set = S.Set
type ByteString = BS.ByteString

newtype Ref a = Ref { unRef :: Word64 } deriving (Enum, Ord, Eq)

newtype InputId = InputId Int deriving (Enum, Ord, Eq)

data Circ = Input InputId
          | Const Bool
          | Not (Ref Circ)
          | Xor (Ref Circ) (Ref Circ)
          | And (Ref Circ) (Ref Circ)
          deriving (Eq, Ord, Show)

-- it is convenient to have a Circ without associated data
data Operation = OInput
               | OConst
               | ONot
               | OXor
               | OAnd
               deriving (Show, Eq, Ord)

data Env c = Env { env_deref :: Map (Ref c) c
                 , env_dedup :: Map c (Ref c)
                 } deriving (Show)

data Program c = Program { prog_inputs  :: Set (Ref c)
                         , prog_outputs :: [Ref c]
                         , prog_env     :: Env c
                         } deriving (Show)

data TruthTable = TTInp InputId
                | TT { tt_f    :: Bool -> Bool -> Bool
                     , tt_inpx :: Ref TruthTable
                     , tt_inpy :: Ref TruthTable
                     }

--------------------------------------------------------------------------------
-- data types for garbling

type Color = Bool

type Wirelabel = BS.ByteString

type WirelabelPair = (Wirelabel, Wirelabel)

type GarbledTable = [((Color,Color), Wirelabel)]

data GarbledGate = GarbledInput InputId
                 | GarbledXor  (Ref GarbledGate) (Ref GarbledGate)
                 | GarbledAnd  (Ref GarbledGate) (Ref GarbledGate) Wirelabel Wirelabel
                 | GarbledGate (Ref GarbledGate) (Ref GarbledGate) GarbledTable
                 deriving (Show, Eq, Ord)

type Garble = StateT (Program GarbledGate)
                (StateT SystemRNG
                  (ReaderT (Program TruthTable)
                    (State Context)))

data Context = Context { ctx_pairs :: Map (Ref GarbledGate) WirelabelPair
                       , ctx_truth :: Map Wirelabel Bool
                       , ctx_key   :: AES
                       , ctx_r     :: Wirelabel
                       , ctx_ctr   :: Int
                       }

--------------------------------------------------------------------------------
-- instances

class CanHaveChildren c where
  children :: c -> [Ref c]

instance CanHaveChildren Circ where
  children (Not x  ) = [x]
  children (Xor x y) = [x,y]
  children (And x y) = [x,y]
  children _         = []

instance CanHaveChildren TruthTable where
  children (TT {tt_inpx = x, tt_inpy = y}) = [x,y]
  children (TTInp _) = []

instance Eq TruthTable where
  TTInp a == TTInp b = a == b
  TTInp _ == TT {..} = False
  TT {..} == TTInp _ = False
  a == b = let z  = tt_inpx a == tt_inpx b && tt_inpy a == tt_inpy b
               xs = zipWith (==) (truthVals (tt_f a)) (truthVals (tt_f b))
           in foldl (&&) z xs

instance Ord TruthTable where
  TTInp a <= TTInp b = a <= b
  TTInp _ <= TT {..} = True
  TT {..} <= TTInp _ = False
  a <= b = let z  = tt_inpx a <= tt_inpx b || tt_inpy a <= tt_inpy b
               xs = zipWith (<=) (truthVals (tt_f a)) (truthVals (tt_f b))
           in foldl (||) z xs

instance Show TruthTable where
  show (TTInp i) = show i
  show (TT {tt_f = f}) = "TT" ++ map bitc (truthVals f)
    where bitc b = if b then '1' else '0'

instance CanHaveChildren GarbledGate where
  children (GarbledInput _)    = []
  children (GarbledGate x y _) = [x,y]
  children (GarbledXor x y)    = [x,y]

instance Show (Ref c) where
  show (Ref x) = "<" ++ show x ++ ">"

instance Show InputId where
  show (InputId i) = "in" ++ show i

--------------------------------------------------------------------------------
-- helpers

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
emptyProg = Program { prog_inputs = S.empty, prog_outputs = [], prog_env = emptyEnv }

emptyEnv :: Env c
emptyEnv = Env { env_deref = M.empty, env_dedup = M.empty }

truthVals :: (Bool -> Bool -> Bool) -> [Bool]
truthVals f = [ f x y | x <- [True, False], y <- [True, False] ]

emptyContext :: Context
emptyContext = Context M.empty M.empty undefined undefined 0

wlp_true :: WirelabelPair -> Wirelabel
wlp_true = snd

wlp_false :: WirelabelPair -> Wirelabel
wlp_false = fst

circ2op :: Circ -> Operation
circ2op (Input _) = OInput
circ2op (Const _) = OConst
circ2op (Not   _) = ONot
circ2op (Xor _ _) = OXor
circ2op (And _ _) = OAnd

