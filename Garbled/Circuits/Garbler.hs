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

type Garble = ReaderT (Env TruthTable) (RandT StdGen (State GarbledCircuit)) -- a handy monad for garbling

runGarble :: Garble a
          -> Env TruthTable
          -> StdGen
          -> GarbledCircuit
runGarble f env gen = execState (evalRandT (runReaderT f env) gen) initialGC
  where initialGC = GarbledCircuit [] [] M.empty

garble :: Program TruthTable -> Garble ()
garble prog = undefined
    {-zipWithM_ inputPair (prog_inputs prog) inpIds-}
    {-mapM_ construct topo-}
    {-modify (\st -> st { gc_outputs = (prog_outputs prog)-}
                      {-, gc_inputs  = (prog_inputs  prog)-}
                      {-})-}
  {-where-}
    {-topo   = topoSort ttRefs prog-}
    {-deref  = env_deref (prog_env prog)-}
    {-inps   = map (flip violentLookup deref) (prog_inputs prog)-}
    {-inpIds = map (\(Input id) -> id) inps-}

construct = undefined
{-construct :: CircRef -> Garble ()-}
{-construct ref = do-}
  {-circ     <- lookupCirc ref-}
  {-children <- mapM lookupGate (circRefs circ)-}
  {-case circ of-}
    {-Not _   -> putGate ref (flipLabel (head children)-}
    {-And _ _ -> garbleAnd ref-}

flipLabel :: WireLabelPair -> WireLabelPair
flipLabel p = WireLabelPair { wl_true  = wl_false p -- flip the true/false wire labels
                            , wl_false = wl_true p  -- actually i don't think I can do this
                            }                       -- maybe need to keep permute bits the same?

{-construct :: CircRef -> Garble WireLabelPair-}
{-construct ref = return pair -}
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

{-lookupGate :: CircRef -> Garble GarbledGate-}
{-lookupGate ref = do-}
  {-maybeGate <- M.lookup ref <$> gets gc_gates-}
  {-case maybeGate of-}
    {-Nothing -> error "[lookupGate] gate doesn't exist"-}
    {-Just g  -> return g-}

{-lookupCirc :: CircRef -> Garble Circuit-}
{-lookupCirc ref = do-}
  {-deref <- asks env_deref-}
  {-case M.lookup ref deref of-}
    {-Nothing -> error "[lookupCirc] no ref"-}
    {-Just c  -> return c-}

