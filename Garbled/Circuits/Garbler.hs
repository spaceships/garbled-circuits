{-# LANGUAGE LambdaCase #-}

module Garbled.Circuits.Garbler where

import Garbled.Circuits.Types
import Garbled.Circuits.Util
import Garbled.Circuits.Plaintext.TruthTable

import           Control.Monad.Random
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Bits (xor)
import           Data.Functor
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

type Garble = StateT (Program GarbledGate)
                (RandT StdGen
                  (ReaderT (Program TruthTable)
                    (State AllTheThings)))

data AllTheThings = AllTheThings { refMap  :: M.Map (Ref TruthTable) (Ref GarbledGate)
                                 , pairMap :: M.Map (Ref GarbledGate) WireLabelPair
                                 }

--------------------------------------------------------------------------------
-- garbling

-- assumes children are already garbled! (use topoSort)
garble :: Ref TruthTable -> Garble (Ref GarbledGate)
garble tt_ref = lookupTT tt_ref >>= \case
  TTInp id -> do
    pair   <- new_wirelabels
    gg_ref <- inputp (GarbledInput pair)
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

new_wirelabels :: Garble WireLabelPair
new_wirelabels = do
  [x0, x1] <- getRandoms :: Garble [Secret]
  c        <- getRandom  :: Garble Color
  return $ WireLabelPair { wl_true = (c, x0), wl_false = (not c, x1) }

encode :: (Bool -> Bool -> Bool) -- the function defining a TruthTable
       -> WireLabelPair          -- the x-wirelabel pair
       -> WireLabelPair          -- the y-wirelabel pair
       -> WireLabelPair          -- the out-wirelabel pair
       -> [((Color, Color), WireLabel)]
encode f x_pair y_pair out_pair = do
    a <- [True, False]
    b <- [True, False]
    let (xcol, x) = sel a x_pair
        (ycol, y) = sel b y_pair
        (zcol, z) = sel (f a b) out_pair
    return ((xcol,ycol), (zcol, enc x y z))
  where
    sel b = if b then wl_true else wl_false

--------------------------------------------------------------------------------
-- helpers

lookupTT :: Ref TruthTable -> Garble TruthTable
lookupTT ref = asks (lookupC ref)

tt2gg_lookup :: Ref TruthTable -> Garble (Ref GarbledGate)
tt2gg_lookup ref = lift $ gets (M.lookup ref . refMap) >>= \case
  Nothing   -> err "tt2gg_lookup" "no ref" [ref]
  Just ref' -> return ref'

pairMap_lookup :: Ref GarbledGate -> Garble WireLabelPair
pairMap_lookup ref = lift $ gets (M.lookup ref . pairMap) >>= \case
  Nothing   -> err "pairMap_lookup" "no ref" [ref]
  Just pair -> return pair

allthethings :: Ref TruthTable -> Ref GarbledGate -> WireLabelPair -> Garble ()
allthethings reftt refgg pair = tt2gg_insert reftt refgg >> pairMap_insert refgg pair

tt2gg_insert :: Ref TruthTable -> Ref GarbledGate -> Garble ()
tt2gg_insert x y = lift $ modify (\st -> st { refMap = M.insert x y (refMap st) })

pairMap_insert :: Ref GarbledGate -> WireLabelPair -> Garble ()
pairMap_insert ref pair = lift $ modify (\st -> st { pairMap = M.insert ref pair (pairMap st) })
