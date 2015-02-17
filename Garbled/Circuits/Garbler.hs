{-# LANGUAGE LambdaCase, NamedFieldPuns #-}

module Garbled.Circuits.Garbler where

import Garbled.Circuits.Types
import Garbled.Circuits.Util
import Garbled.Circuits.Plaintext.TruthTable

import           Control.Applicative
import           Control.Monad.Random
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Bits (xor)
import           Data.Hashable
import qualified Data.Map as M
import           System.Random

-- TODO: add optimizations: point-and-permute, free-xor, row-reduction, half-gates
-- TODO: get better random numbers

--------------------------------------------------------------------------------
-- encryption and decryption for wirelabels

enc :: Int -> Int -> Int -> Int
enc k1 k2 m = hash (k1, k2) `xor` m

dec :: Int -> Int -> Int -> Int
dec = enc

--------------------------------------------------------------------------------
-- data types for garbling

type PairMap = M.Map (Ref GarbledGate) WirelabelPair

type Garble = StateT (Program GarbledGate)
                (RandT StdGen
                  (ReaderT (Program TruthTable)
                    (State AllTheThings)))

data AllTheThings = AllTheThings { refMap   :: M.Map (Ref TruthTable) (Ref GarbledGate)
                                 , pairMap  :: PairMap
                                 } deriving (Show)

--------------------------------------------------------------------------------
-- garbling

garbleTT :: Program TruthTable -> IO (Program GarbledGate, AllTheThings)
garbleTT prog_tt = do
    gen <- getStdGen
    let ((outs, prog_gg), things) = runGarble gen (mapM garble topo)
        prog_gg' = prog_gg { prog_outputs = outs }
    return (prog_gg', things)
  where
    runAllThingsSt m = runState m (AllTheThings M.empty M.empty) -- (a, allthethings)
    runTTReader    m = runReaderT m prog_tt -- a
    runRand gen    m = evalRandT m gen -- a
    runProgSt      m = runStateT m emptyProg -- (a, Program GarbledGate)

    runGarble :: StdGen -> Garble a -> ((a, Program GarbledGate), AllTheThings)
    runGarble gen g = runAllThingsSt (runTTReader (runRand gen (runProgSt g)))

    topo = topoSort prog_tt

-- assumes children are already garbled! (use topoSort)
garble :: Ref TruthTable -> Garble (Ref GarbledGate)
garble tt_ref = lookupTT tt_ref >>= \case
  TTInp id -> do
    pair   <- new_wirelabels
    gg_ref <- inputp (GarbledInput id)
    allthethings tt_ref gg_ref pair
    return gg_ref
  tt -> do
    xref   <- tt2gg_lookup (tt_inpx tt)
    yref   <- tt2gg_lookup (tt_inpy tt)
    x_wl   <- pairMap_lookup xref
    y_wl   <- pairMap_lookup yref
    out_wl <- new_wirelabels
    let table = encode (tt_f tt) x_wl y_wl out_wl
    gg_ref <- internp (garbledGate xref yref table)
    allthethings tt_ref gg_ref out_wl
    return gg_ref

new_wirelabels :: Garble WirelabelPair
new_wirelabels = do
  x <- getRandom :: Garble Int
  y <- getRandom :: Garble Int
  c <- getRandom :: Garble Color
  let wlt = Wirelabel { wl_col = c,     wl_val = x }
      wlf = Wirelabel { wl_col = not c, wl_val = y }
  return $ WirelabelPair { wlp_true = wlt, wlp_false = wlf }

encode :: (Bool -> Bool -> Bool) -- the function defining a TruthTable
       -> WirelabelPair          -- the x-wirelabel pair
       -> WirelabelPair          -- the y-wirelabel pair
       -> WirelabelPair          -- the out-wirelabel pair
       -> [((Color, Color), Wirelabel)]
encode f x_pair y_pair out_pair = do
    a <- [True, False]
    b <- [True, False]
    let x = sel a x_pair
        y = sel b y_pair
        z = sel (f a b) out_pair
        out = z { wl_val = enc (wl_val x) (wl_val y) (wl_val z) }
    return ((wl_col x, wl_col y), out)

--------------------------------------------------------------------------------
-- evaluator

evalGG :: Program GarbledGate
       -> PairMap
       -> [Bool]
       -> [Bool]
evalGG prog pairMap inps = map ungarble outmap
  where
    inpwlps  = map (flip violentLookup pairMap) (prog_inputs prog)
    outwlps  = map (flip violentLookup pairMap) (prog_outputs prog)
    inpwires = sel <$> inps <*> inpwlps
    inputs   = zip (map InputId [0..]) inpwires
    result   = evalProg reconstruct prog inpwires :: [Wirelabel]
    outmap   = zip result outwlps                 :: [(Wirelabel, WirelabelPair)]

    reconstruct :: GarbledGate -> [Wirelabel] -> Wirelabel
    reconstruct (GarbledInput id) [] = case lookup id inputs of
      Nothing -> err "reconstruct" "no input wire with id" [id]
      Just wl -> wl
    reconstruct g [x,y] = case lookup (wl_col x, wl_col y) (gate_table g) of
      Nothing -> err "reconstruct" "no color matching" [wl_col x, wl_col y]
      Just z  -> z { wl_val = dec (wl_val y) (wl_val x) (wl_val z) }
    reconstruct _ _ = err "reconstruct" "unknown pattern" [-1]

    ungarble :: (Wirelabel, WirelabelPair) -> Bool
    ungarble (wl, wlp) = if wlp_true  wlp == wl then True  else
                         if wlp_false wlp == wl then False else
                         err "ungarble" "unknown wirelabel" [wl]

--------------------------------------------------------------------------------
-- helpers

lookupTT :: Ref TruthTable -> Garble TruthTable
lookupTT ref = asks (lookupC ref)

tt2gg_lookup :: Ref TruthTable -> Garble (Ref GarbledGate)
tt2gg_lookup ref = lift $ gets (M.lookup ref . refMap) >>= \case
  Nothing   -> err "tt2gg_lookup" "no ref" [ref]
  Just ref' -> return ref'

pairMap_lookup :: Ref GarbledGate -> Garble WirelabelPair
pairMap_lookup ref = lift $ gets (M.lookup ref . pairMap) >>= \case
  Nothing   -> err "pairMap_lookup" "no ref" [ref]
  Just pair -> return pair

allthethings :: Ref TruthTable
             -> Ref GarbledGate
             -> WirelabelPair
             -> Garble ()
allthethings reftt refgg pair = do
  tt2gg_insert reftt refgg
  pairMap_insert refgg pair

tt2gg_insert :: Ref TruthTable -> Ref GarbledGate -> Garble ()
tt2gg_insert x y =
  lift $ modify (\st -> st { refMap = M.insert x y (refMap st) })

pairMap_insert :: Ref GarbledGate -> WirelabelPair -> Garble ()
pairMap_insert ref pair =
  lift $ modify (\st -> st { pairMap = M.insert ref pair (pairMap st) })

sel :: Bool -> WirelabelPair -> Wirelabel
sel b = if b then wlp_true else wlp_false
