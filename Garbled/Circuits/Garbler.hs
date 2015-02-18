{-# LANGUAGE LambdaCase, NamedFieldPuns #-}

module Garbled.Circuits.Garbler where

import Garbled.Circuits.Types
import Garbled.Circuits.Util
import Garbled.Circuits.Plaintext.TruthTable

import           Control.Monad.Random
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Bits (xor)
import           Data.Hashable
import qualified Data.Map as M
import qualified Data.Set as S
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

data AllTheThings = AllTheThings { things_refs  :: Map (Ref TruthTable) (Ref GarbledGate)
                                 , things_pairs :: Map (Ref GarbledGate) WirelabelPair
                                 , things_truth :: Map Wirelabel Bool
                                 } deriving (Show)

--------------------------------------------------------------------------------
-- garbling

tt2gg :: Program TruthTable -> IO (Program GarbledGate, AllTheThings)
tt2gg prog_tt = do
    gen <- getStdGen
    let (prog_gg, things) = runGarble gen (mapM_ garbleGate (prog_outputs prog_tt))
        outs     = map (violentLookup $ things_refs things) (prog_outputs prog_tt)
        prog_gg' = prog_gg { prog_outputs = outs }
    return (prog_gg', things)
  where
    runAllThingsSt m = runState m (AllTheThings M.empty M.empty M.empty) -- (a, allthethings)
    runTTReader    m = runReaderT m prog_tt -- a
    runRand gen    m = evalRandT m gen -- a
    runProgSt      m = execStateT m emptyProg -- (a, Program GarbledGate)

    runGarble :: StdGen -> Garble a -> (Program GarbledGate, AllTheThings)
    runGarble gen g = runAllThingsSt (runTTReader (runRand gen (runProgSt g)))


    {-topo = topoSort prog_tt-}

--todo: basecase - already garbled tt_ref
garbleGate :: Ref TruthTable -> Garble (Ref GarbledGate)
garbleGate tt_ref = tt2gg_lookup tt_ref >>= \case -- if the TruthTable already is garbled
  Just ref -> return ref                          -- return a ref to it
  Nothing  -> lookupTT tt_ref >>= \case           -- otherwise get the TruthTable
    TTInp id -> do                                -- if it's an input:
      pair   <- new_wirelabels                    --   get new wirelabels
      gg_ref <- inputp (GarbledInput id)          --   make it a gate, get a ref
      allthethings tt_ref gg_ref pair             --   keep track of our work in the state
      return gg_ref                               --   return the gate ref
    tt -> do                                      -- otherwise:
      maybeXref <- tt2gg_lookup (tt_inpx tt)      -- check if the left arg has been garbled
      xref <- case maybeXref of                  
        Nothing  -> garbleGate (tt_inpx tt)       --   if it hasn't, garble it
        Just ref -> return ref                    --   return the new ref
      maybeYref <- tt2gg_lookup (tt_inpy tt)
      yref <- case maybeYref of
        Nothing  -> garbleGate (tt_inpy tt)
        Just ref -> return ref
      x_wl   <- pairs_lookup xref
      y_wl   <- pairs_lookup yref
      out_wl <- new_wirelabels
      let table = encode (tt_f tt) x_wl y_wl out_wl
      gg_ref <- internp (GarbledGate xref yref table)
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

evalGG :: [Bool] -> (Program GarbledGate, AllTheThings) -> [Bool]
evalGG inps (prog, things) = map ungarble result
  where
    inpwlps  = map (violentLookup $ things_pairs things) (S.toList $ prog_inputs prog)
    inpwires = zipWith sel inps inpwlps
    inputs   = zip (map InputId [0..]) inpwires

    result = evalProg reconstruct prog inpwires :: [Wirelabel]

    reconstruct :: GarbledGate -> [Wirelabel] -> Wirelabel
    reconstruct (GarbledInput id) [] = case lookup id inputs of
      Nothing -> err "reconstruct" "no input wire with id" [id]
      Just wl -> wl
    reconstruct g [x,y] = case lookup (wl_col x, wl_col y) (gate_table g) of
      Nothing -> err "reconstruct" "no color matching" [wl_col x, wl_col y]
      Just z  -> z { wl_val = dec (wl_val x) (wl_val y) (wl_val z) }
    reconstruct _ _ = err "reconstruct" "unknown pattern" [-1]

    ungarble :: Wirelabel -> Bool
    ungarble wl = case M.lookup wl (things_truth things) of
      Nothing -> err "ungarble" "unknown wirelabel" [wl]
      Just b  -> b

--------------------------------------------------------------------------------
-- helpers

lookupTT :: Ref TruthTable -> Garble TruthTable
lookupTT ref = asks (lookupC ref)

tt2gg_lookup :: Ref TruthTable -> Garble (Maybe (Ref GarbledGate))
tt2gg_lookup ref = lift $ gets (M.lookup ref . things_refs)

pairs_lookup :: Ref GarbledGate -> Garble WirelabelPair
pairs_lookup ref = lift $ gets (M.lookup ref . things_pairs) >>= \case
  Nothing   -> err "pairs_lookup" "no ref" [ref]
  Just pair -> return pair

allthethings :: Ref TruthTable -> Ref GarbledGate -> WirelabelPair -> Garble ()
allthethings reftt refgg pair = do
  tt2gg_insert reftt refgg
  pairs_insert refgg pair
  truth_insert (wlp_true  pair) True
  truth_insert (wlp_false pair) False

tt2gg_insert :: Ref TruthTable -> Ref GarbledGate -> Garble ()
tt2gg_insert x y =
  lift $ modify (\st -> st { things_refs = M.insert x y (things_refs st) })

pairs_insert :: Ref GarbledGate -> WirelabelPair -> Garble ()
pairs_insert ref pair =
  lift $ modify (\st -> st { things_pairs = M.insert ref pair (things_pairs st) })

truth_insert :: Wirelabel -> Bool -> Garble ()
truth_insert l b = 
  lift $ modify (\st -> st { things_truth = M.insert l b (things_truth st) })

sel :: Bool -> WirelabelPair -> Wirelabel
sel b = if b then wlp_true else wlp_false
