{-# LANGUAGE PackageImports, GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Crypto.GarbledCircuits.Types where

import           Control.Applicative hiding (Const)
import           Control.Monad.Reader
import           Control.Monad.State (StateT, State)
import           Crypto.Cipher.AES
import           "crypto-random" Crypto.Random
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Serialize
import           Data.Word
import           Debug.Trace
import           Numeric (showHex)

type Map = M.Map
type Set = S.Set
type ByteString = BS.ByteString

newtype Ref a = Ref { unRef :: Word16 } deriving (Enum, Ord, Eq)

data Party = A | B deriving (Enum, Ord, Eq, Show)

newtype InputId = InputId { getInputId :: Int } deriving (Enum, Ord, Eq)

data Circ = Input InputId Party
          | Const Bool
          | Not (Ref Circ)
          | Xor (Ref Circ) (Ref Circ)
          | And (Ref Circ) (Ref Circ)
          | Or  (Ref Circ) (Ref Circ)
          deriving (Eq, Ord, Show)

-- it is convenient to have a Circ without associated data
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

type WirelabelPair = (Wirelabel, Wirelabel)

data GarbledGate = GarbledInput InputId Party
                 | FreeXor  (Ref GarbledGate) (Ref GarbledGate)
                 | HalfGate (Ref GarbledGate) (Ref GarbledGate) Wirelabel Wirelabel
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

instance Serialize (Ref GarbledGate) where
  put = put . unRef
  get = Ref <$> get

instance Serialize GarbledGate where
  put (GarbledInput i p) = do
    put (0            :: Word8)
    put (getInputId i :: Int)
    put (fromEnum p   :: Int)

  put (FreeXor x y) = do
    put (1 :: Word8)
    put x
    put y

  put (HalfGate x y g e) = do
    put (2 :: Word8)
    put x
    put y
    put g
    put e

  get = do
    ty <- get :: Get Word8
    case ty of
      0 -> GarbledInput <$> (InputId <$> get) <*> (toEnum <$> get)
      1 -> FreeXor <$> get <*> get
      2 -> HalfGate <$> get <*> get <*> get <*> get
      _ -> error "[get] unknown garbled gate type"

instance Serialize (Program GarbledGate) where
  put prog = do
    put $ S.toList (prog_input_a prog)
    put $ S.toList (prog_input_b prog)
    put $ prog_output prog
    put $ map snd (M.toList (prog_env prog))
  get = do
    inpA  <- S.fromList <$> get
    inpB  <- S.fromList <$> get
    outs  <- get
    gates <- get :: Get [GarbledGate]
    let env = M.fromList $ zip (map Ref [0..]) gates
    let prog =  emptyProg { prog_input_a = inpA
                          , prog_input_b = inpB
                          , prog_output  = outs
                          , prog_env     = env
                          }
    traceM ("*******" ++ show prog)
    return prog

--------------------------------------------------------------------------------
-- helpers

prog_inputs :: Party -> Program c -> Set (Ref c)
prog_inputs A = prog_input_a
prog_inputs B = prog_input_b

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

circ2op :: Circ -> Operation
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