module Garbled.Circuits.Garbler where

import Garbled.Circuits.Plaintext.Rewrite (foldConsts, topoSort)
import Garbled.Circuits.Plaintext.Types
import Garbled.Circuits.Util

import Data.Functor
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Random
import Data.Hashable
import System.Random
import qualified Data.Map as M

-- TODO: get better random numbers!
-- TODO: add optimizations: point-and-permute, free-xor, row-reduction, half-gates

enc :: Int -> Int -> Int -> Int
enc k1 k2 m = hash (k1, k2, m)

type Secret    = Int
type Color     = Bool
type WireLabel = (Secret, Color)

data WireLabelPair = WireLabelPair { wl_true  :: WireLabel
                                   , wl_false :: WireLabel
                                   } deriving (Show)

data GarbledCircuit = GarbledCircuit { gc_inputs  :: [CircRef]
                                     , gc_outputs :: [CircRef]
                                     , gc_gates   :: Map CircRef GarbledGate
                                     } deriving (Show)

data GarbledGate = GarbledInput WireLabelPair
                 | GarbledGate { gate_inLeft  :: CircRef
                               , gate_inRight :: CircRef
                               , gate_table   :: (WireLabel, WireLabel, WireLabel, WireLabel) -- 11, 10, 01, 00
                               } deriving (Show)

type Garble = ReaderT CircuitEnv (RandT StdGen (State GarbledCircuit)) -- a handy monad for garbling

runGarble :: Garble a
          -> CircuitEnv
          -> StdGen
          -> GarbledCircuit
runGarble f env gen = execState (evalRandT (runReaderT f env) gen) initialGC
  where initialGC = GarbledCircuit [] [] M.empty

garble :: Program -> Garble ()
garble p = do
    zipWithM_ inputPair (prog_inputs prog) inpIds
    mapM_ construct topo
    modify (\st -> st { gc_outputs = (prog_outputs prog)
                      , gc_inputs  = (prog_inputs  prog)
                      })
  where
    prog   = foldConsts prog
    topo   = topoSort prog
    deref  = env_deref (prog_env prog)
    inps   = map (flip violentLookup deref) (prog_inputs prog)
    inpIds = map (\(Input id) -> id) inps

lookupGate :: CircRef -> Garble GarbledGate
lookupGate ref = do
  maybeGate <- M.lookup ref <$> gets gc_gates
  case maybeGate of
    Nothing -> error "[lookupGate] gate doesn't exist"
    Just g  -> return g

lookupCirc :: CircRef -> Garble Circuit
lookupCirc = undefined

construct :: CircRef -> Garble ()
construct ref = undefined

{-construct :: CircRef -> Garble WireLabelPair-}
{-construct ref = return pair -}
  {-where pair = WireLabelPair { wl_true  = wl_false x -- flip the true/false wire labels-}
                             {-, wl_false = wl_true x  -- actually i don't think I can do this-}
                             {-}                       -- maybe need to keep permute bits the same?-}
{-[>construct (Xor _ _) [x,y] = Data.Bits.xor x y<]-}
{-[>construct (And _ _) [x,y] = x && y<]-}
{-[>construct (Or _ _)  [x,y] = x || y<]-}

inputPair :: CircRef -> InputId -> Garble WireLabelPair
inputPair ref id = do
  [x0, x1] <- getRandoms :: Garble [Int]
  c        <- getRandom  :: Garble Color
  let pair = WireLabelPair { wl_true = (x0, c), wl_false = (x1, not c) }
  putGate ref (GarbledInput pair)
  return pair

putGate :: CircRef -> GarbledGate -> Garble ()
putGate ref gate = modify (\st -> st { gc_gates = M.insert ref gate (gc_gates st) })
