{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Crypto.GarbledCircuits.Types where

import           Control.Monad.Reader
import           Control.Monad.State (StateT)
import           Crypto.Cipher.AES128
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.IORef

type Map = M.Map
type Set = S.Set
type ByteString = BS.ByteString

-- |'Ref' is a way to associate ids with some kind of gate. Generally, we think
-- of a Ref as representing the output of the gate.
newtype Ref a = Ref { unRef :: Int } deriving (Enum, Ord, Eq)

-- |A garbled circuit is an asymmetric two-party protocol. The 'Garbler' creates
-- wirelabels for each gate, and sends them to the 'Evaluator'. The 'Evaluator'
-- decrypts the circuit and reports the output.
data Party = Garbler | Evaluator deriving (Enum, Ord, Eq, Show)

newtype InputId = InputId { getInputId :: Int } deriving (Enum, Ord, Eq)

-- |A 'Circuit' is the fundamental description of the program that is executed
-- in a garbled circuit.
--
-- Use the constructors in "Crypto.GarbledCircuits.Language" to create your own
-- circuits.
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

-- |'Program' keeps track of inputs, outputs, and references.
--
-- It is polymorphic over 'Circuit', 'TruthTable', and 'GarbledGate'.
data Program c = Program {
                           -- | Input bits belonging to the 'Garbler'
                           prog_input_gb :: Set (Ref c)
                           -- | Input bits belonging to the 'Evaluator'
                         , prog_input_ev :: Set (Ref c)
                           -- | The output bits, in order.
                         , prog_output   :: [Ref c]
                           -- | A mapping of Refs to 'c'
                         , prog_env      :: Env c
                         } deriving (Show, Eq)

-- |'TruthTable' is a plaintext representation of a 'Circuit' where all gates
-- are binary. To make it we must fold 'Const' and 'Not' gates into the binary
-- ('Xor', 'And', 'Or') gates above them. This is necessary because only binary
-- gates are garbleable in our scheme.
data TruthTable = TTInp InputId Party
                | TT {
                       -- | The type of this gate.
                       tt_f    :: Operation
                       -- | A ref to the left input gate.
                     , tt_inpx :: Ref TruthTable
                       -- | A ref to the right input gate.
                     , tt_inpy :: Ref TruthTable
                       -- | Whether the left input should be negated.
                     , tt_negx :: Bool
                       -- | Whether the right input should be negated.
                     , tt_negy :: Bool
                     } deriving (Ord, Eq, Show)

--------------------------------------------------------------------------------
-- data types for garbling

-- | 'Wirelabel' is simply 16 byte long 'ByteString' (128 bits).
type Wirelabel = BS.ByteString

-- | 'WirelabelPair' is a mapping of 'Wirelabel's to 'True' and 'False'
data WirelabelPair = WirelabelPair { wlp_false :: Wirelabel
                                   , wlp_true  :: Wirelabel
                                   } deriving (Show)

-- |A 'GarbledGate' is either an input, a free xor, or a half gate.
--
-- Inputs are placeholders in 'GarbledGate'. At garbletime, two random 128-bit strings called
-- 'Wirelabel's are chosen for each input bit. One 'Wirelabel' corresponds to 'True' and the
-- other to 'False'. The 'Garbler' knows the truth-values for all 'Wirelabel's. The 'Evaluator'
-- learns the 'Wirelabel's for each of its inputs, but cannot guess what the other wirelabels
-- are since they are 128-bit random strings.
--
-- 'Xor' gates are free in the sense that they require no communication. The evaluator simply
-- xors its input wires to get the correct result. See
-- <https://web.engr.oregonstate.edu/~rosulekm/scbib/index.php?n=Paper.KS08> for details.
--
-- 'And' and 'Or' gates map to 'HalfGates'. Half gates contain the only information in a garbled
-- circuit that the 'Garbler' needs to send to the 'Evaluator'. Namely, two 'Wirelabels' per
-- gate.  Half gates are a very recent innovation. See ZRE15 <http://eprint.iacr.org/2014/756>
-- for details.
data GarbledGate = GarbledInput InputId Party
                 | FreeXor  (Ref GarbledGate) (Ref GarbledGate)
                 | HalfGate (Ref GarbledGate) (Ref GarbledGate) Wirelabel Wirelabel
                 deriving (Show, Eq, Ord)

-- |A monad for creating garbled circuits.
type Garble = StateT (Program GarbledGate)
                (ReaderT (Program TruthTable)
                  (StateT Context
                    IO))

-- |'Context' contains state and proprietary information for garbling.
data Context = Context {
                         -- | The output wires for each gate.
                         ctx_pairs :: Map (Ref GarbledGate) WirelabelPair
                         -- | The truth value of each wire.
                       , ctx_truth :: Map Wirelabel Bool
                         -- | The AES key for the hash function.
                       , ctx_key   :: AESKey128
                         -- | The @R@ value. Necessary for free-xor and half-gates.
                       , ctx_r     :: Wirelabel
                         -- | The @ctr@ value. Used in half-gates.
                       , ctx_ctr   :: Int
                       }

--------------------------------------------------------------------------------
-- network

type Port = Int

data Connection = Connection { conn_send :: ByteString -> IO ()
                             , conn_recv :: Int -> IO ByteString
                             , conn_bytes_sent     :: IORef Int
                             , conn_bytes_received :: IORef Int
                             }

--------------------------------------------------------------------------------
-- instances

-- | 'CanHaveChildren' allows us to write polymorphic traversal functions.
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

-- | Helper for choosing 'Program' input accessors.
prog_inputs :: Party -> Program c -> Set (Ref c)
prog_inputs Garbler   = prog_input_gb
prog_inputs Evaluator = prog_input_ev

-- | A 'Wirelabel' that contains only zeroes.
zeroWirelabel :: Wirelabel
zeroWirelabel = BS.replicate 16 0

emptyEnv :: Env c
emptyEnv = M.empty

emptyContext :: Context
emptyContext = Context M.empty M.empty undefined undefined 0

emptyProg :: Program c
emptyProg = Program { prog_input_gb = S.empty
                    , prog_input_ev = S.empty
                    , prog_output   = []
                    , prog_env      = emptyEnv
                    }
