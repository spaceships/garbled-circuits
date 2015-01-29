module Garbled.Circuits.Garbler where

import Garbled.Circuits.Circuits
import Garbled.Circuits.Util

import Data.Functor
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Random
import Data.Hashable
import System.Random
import qualified Data.Map as M

-- TODO: get better random numbers!

type Color     = Bool                   -- permutation bit for point-and-permute
type WireLabel = (Int, Color)

data WireLabelPair = WireLabelPair { wl_true  :: WireLabel
                                   , wl_false :: WireLabel
                                   } deriving (Show)

data GarbledCircuit = GarbledCircuit { gc_inputs  :: Map InputId WireLabelPair
                                     , gc_gates   :: Map Ref WireLabelPair
                                     } deriving (Show)

type Garble = ReaderT CircuitEnv (RandT StdGen (State GarbledCircuit)) -- a handy monad for garbling

runGarble :: Garble a
          -> CircuitEnv
          -> StdGen
          -> (a, GarbledCircuit)
runGarble f env gen = runState (evalRandT (runReaderT f env) gen) (GarbledCircuit M.empty M.empty)

garble :: Program -> Garble [WireLabelPair]
garble prog = do
    zipWithM_ inputPair (prog_inputs prog) ids
    outputs <- mapM traverse (prog_outputs prog)
    return outputs
  where
    deref = env_deref (prog_env prog)
    inps = map (flip violentLookup deref) (prog_inputs prog)
    ids  = map (\(Input id) -> id) inps

-- unary gates need to be folded upwards, hence the return value may be a circuit
traverse :: Ref -> Garble WireLabelPair
traverse ref = do
  precomputed <- get
  case M.lookup ref (gc_gates precomputed) of
    Just p  -> return p
    Nothing -> do
      env <- ask
      let circ = violentLookup ref (env_deref env)
      children <- mapM traverse (circRefs circ)
      result <- construct ref circ children
      putLabel ref result
      return result

construct :: Ref 
          -> Circuit 
          -> [WireLabelPair] 
          -> Garble WireLabelPair
construct ref (Input id) [] = inputPair ref id
construct ref (Not _)   [x] = return pair 
  where pair = WireLabelPair { wl_true  = wl_false x -- flip the true/false wire labels
                             , wl_false = wl_true x  -- actually i don't think I can do this
                             }                       -- maybe need to keep permute bits the same?
{-construct (Xor _ _) [x,y] = Data.Bits.xor x y-}
{-construct (And _ _) [x,y] = x && y-}
{-construct (Or _ _)  [x,y] = x || y-}

inputPair :: Ref -> InputId -> Garble WireLabelPair
inputPair ref id = do
  [x0, x1] <- getRandoms :: Garble [Int]
  [c0, c1] <- getRandoms :: Garble [Bool]
  let pair = WireLabelPair { wl_true = (x0, c0), wl_false = (x1, c1) }
  putLabel ref pair
  putInputId id pair
  return pair

putLabel :: Ref -> WireLabelPair -> Garble ()
putLabel ref pair = modify (\st -> st { gc_gates = M.insert ref pair (gc_gates st) })

putInputId :: InputId -> WireLabelPair -> Garble ()
putInputId id pair = modify (\st -> st { gc_inputs = M.insert id pair (gc_inputs st) })

