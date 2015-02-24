{-# LANGUAGE LambdaCase, NamedFieldPuns #-}

module Crypto.GarbledCircuits.GarbledGate 
  (
    tt2gg
  , garble
  , evalGG
  )
where

import Crypto.GarbledCircuits.Types
import Crypto.GarbledCircuits.Util
import Crypto.GarbledCircuits.TruthTable

import           Data.Functor
import           Data.Maybe
import           Control.Monad.Random
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Bits
import           Data.Hashable
import qualified Data.Map as M
import qualified Data.Set as S
import           System.Random

-- TODO: add optimizations: point-and-permute, free-xor, row-reduction, half-gates
-- TODO: get better random numbers
-- TODO: get better encryption

--------------------------------------------------------------------------------
-- encryption and decryption for wirelabels

enc :: Bits a => a -> a -> a -> a
enc k1 k2 m = k1 `xor` k2 `xor` m

dec :: Bits a => a -> a -> a -> a
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

garble :: Program Circ -> IO (Program GarbledGate, AllTheThings)
garble = tt2gg . circ2tt

tt2gg :: Program TruthTable -> IO (Program GarbledGate, AllTheThings)
tt2gg prog_tt = do
    gen <- getStdGen
    let (prog_gg, things) = runGarble gen $ do
          mapM_ garbleGate (S.toList $ prog_inputs prog_tt)
          mapM_ garbleGate (prog_outputs prog_tt)
        outs     = map (violentLookup $ things_refs things) (prog_outputs prog_tt)
        inps     = S.map (violentLookup $ things_refs things) (prog_inputs prog_tt)
        prog_gg' = prog_gg { prog_outputs = outs, prog_inputs = inps }
    return (prog_gg', things)
  where
    runGarble :: StdGen -> Garble a -> (Program GarbledGate, AllTheThings)
    runGarble gen = flip runState (AllTheThings M.empty M.empty M.empty)
                  . flip runReaderT prog_tt
                  . flip evalRandT gen
                  . flip execStateT emptyProg


garbleGate :: Ref TruthTable -> Garble (Ref GarbledGate)
garbleGate tt_ref = tt2gg_lookup tt_ref >>= \case       -- if the TruthTable already is garbled
  Just ref -> return ref                                -- return a ref to it
  Nothing  -> lookupTT tt_ref >>= \case                 -- otherwise get the TruthTable
    TTInp id -> do                                      -- if it's an input:
      pair   <- new_wirelabels                          --   get new wirelabels
      gg_ref <- inputp (GarbledInput id)                --   make it a gate, get a ref
      allthethings tt_ref gg_ref pair                   --   show our work in the state
      return gg_ref                                     --   return the gate ref
    tt -> do                                            -- otherwise:
      xref <- maybeRecurse (tt_inpx tt)                 --   get a ref for the left child gate
      yref <- maybeRecurse (tt_inpy tt)                 --   get a ref for the right child gate
      x_wl <- pairs_lookup xref                         --   lookup wirelabels for left child
      y_wl <- pairs_lookup yref                         --   lookup wirelabels for right child
      out_wl <- new_wirelabels                          --   get new wirelabels
      let table = encode (tt_f tt) x_wl y_wl out_wl     --   create the garbled table
      gg_ref <- internp (GarbledGate xref yref table)   --   add garbled table to the Prog
      allthethings tt_ref gg_ref out_wl                 --   show our work in the state
      return gg_ref                                     --   return the new gate ref

maybeRecurse :: Ref TruthTable -> Garble (Ref GarbledGate)
maybeRecurse tt_ref = tt2gg_lookup tt_ref >>= \case
    Nothing     -> garbleGate tt_ref
    Just gg_ref -> return gg_ref

new_wirelabels :: Garble WirelabelPair
new_wirelabels = do
    x <- getRandom :: Garble Secret
    y <- getRandom :: Garble Secret
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

evalGG :: [Bool] -> (Program GarbledGate, AllTheThings) -> IO [Bool]
evalGG inps (prog, things) = do
    result <- evalProg reconstruct prog inpwires :: IO [Wirelabel]
#ifdef DEBUG
    let out_pairs  = map (\ref -> (ref, violentLookup (things_pairs things) ref)) (prog_outputs prog)
        out_truths = map (\(ref, pair) -> (ref, wl_val (wlp_true pair), wl_val (wlp_false pair))) out_pairs
    forM out_truths $ \(ref, t, f) -> do
      putStrLn ("[evalProg] outwire " ++ show ref ++ " true  value: " ++ show t)
      putStrLn ("[evalProg] outwire " ++ show ref ++ " false value: " ++ show f)
#endif
    return (map ungarble result)
  where
    inpwlps  = map (violentLookup $ things_pairs things) (S.toList $ prog_inputs prog)
    inpwires = zipWith sel inps inpwlps
    inputs   = zip (map InputId [0..]) inpwires

    reconstruct :: GarbledGate -> [Wirelabel] -> IO Wirelabel
    reconstruct (GarbledInput id) [] = case lookup id inputs of
      Nothing -> err "reconstruct" ("no input wire with id " ++ show id ++ "\n" ++ show inputs)
      Just wl -> return wl
    reconstruct g [x,y] = case lookup (wl_col x, wl_col y) (gate_table g) of
      Nothing -> err "reconstruct" "no matching color"
      Just z  -> do
        let new_val = dec (wl_val x) (wl_val y) (wl_val z) 
            new_wl  = z { wl_val = new_val }
#ifdef DEBUG
        checkValueExists new_wl
#endif
        return new_wl
    reconstruct _ _ = err "reconstruct" "unknown pattern"

    ungarble :: Wirelabel -> Bool
    ungarble wl = case M.lookup wl (things_truth things) of
      Nothing -> err "ungarble" $ "unknown wirelabel: " ++ show wl 
#ifdef DEBUG
                                  ++ "\n" ++ showEnv prog ++ showPairs things
#endif
      Just b  -> b

    checkValueExists :: Wirelabel -> IO ()
    checkValueExists wl = case M.lookup wl (things_truth things) of
      Nothing -> putStrLn ("[checkValueExists] warning: unknown wirelabel: " ++ show wl)
      Just b  -> return ()

--------------------------------------------------------------------------------
-- helpers

lookupTT :: Ref TruthTable -> Garble TruthTable
lookupTT ref = asks (lookupC ref)

tt2gg_lookup :: Ref TruthTable -> Garble (Maybe (Ref GarbledGate))
tt2gg_lookup ref = lift $ gets (M.lookup ref . things_refs)

pairs_lookup :: Ref GarbledGate -> Garble WirelabelPair
pairs_lookup ref = lift $ gets (M.lookup ref . things_pairs) >>= \case
  Nothing   -> err "pairs_lookup" ("no ref: " ++ show ref)
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

showEnv :: Program GarbledGate -> String
showEnv prog = 
    "--------------------------------------------------------------------------------\n"
    ++ "-- env \n" ++ concatMap showGate (M.toList (env_deref (prog_env prog))) 
  where
    showGate (ref, gg) = show ref ++ ": " ++ case gg of 
        GarbledInput id -> show id ++ "\n"
        _ -> show (gate_inpx gg) ++ " " ++ show (gate_inpy gg) ++ "\n"
             ++ concatMap showTabElem (gate_table gg)
    showTabElem (col, wl) = "\t" ++ showColor col ++ " " ++ show wl ++ "\n"
    showColor (b0, b1) = (if b0 then "1" else "0") ++ if b1 then "1" else "0"

showPairs :: AllTheThings -> String
showPairs things = 
    "--------------------------------------------------------------------------------\n"
    ++ "-- pairs \n" ++ concatMap showPair (M.toList (things_pairs things)) 
  where
    showPair (ref, pair) = show ref ++ ": true=" ++ show (wlp_true pair) 
                                    ++ " false=" ++ show (wlp_false pair) ++ "\n"
