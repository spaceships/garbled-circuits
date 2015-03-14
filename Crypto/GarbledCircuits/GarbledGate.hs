{-# LANGUAGE PackageImports, LambdaCase, NamedFieldPuns #-}

module Crypto.GarbledCircuits.GarbledGate
where

import Crypto.GarbledCircuits.Encryption
import Crypto.GarbledCircuits.TruthTable
import Crypto.GarbledCircuits.Types
import Crypto.GarbledCircuits.Util

import           Control.Monad.Reader
import           Control.Monad.State
import           Crypto.Cipher.AES
import           "crypto-random" Crypto.Random
import           Data.Functor
import qualified Data.Map         as M
import qualified Data.Set         as Set

--------------------------------------------------------------------------------
-- TODO: add optimizations: row-reduction, half-gates

--------------------------------------------------------------------------------
-- garble a truthtable program

garble :: Program Circ -> IO (Program GarbledGate, Context)
garble = tt2gg . circ2tt

runGarble :: SystemRNG -> Program TruthTable -> Garble a -> (Program GarbledGate, Context)
runGarble gen prog_tt g = let ((_, p), c) = runGarble' gen prog_tt g in (p, c)

runGarble' :: SystemRNG -> Program TruthTable -> Garble a -> ((a, Program GarbledGate), Context)
runGarble' gen prog_tt = 
    flip runState emptyContext
  . flip runReaderT prog_tt
  . flip evalStateT gen
  . flip runStateT emptyProg

tt2gg :: Maybe (Program TruthTable) -> IO (Program GarbledGate, Context)
tt2gg Nothing        = err "tt2gg" "received failed Program TruthTable"
tt2gg (Just prog_tt) = do
    gen <- cprgCreate <$> createEntropyPool
    let (prog_gg, ctx) = runGarble gen prog_tt $ do
          updateKey =<< genKey
          updateR   =<< genR
          -- TT refs are topologically ordered
          mapM_ garbleGate (M.keys (env_deref (prog_env prog_tt)))
        outs     = map convertRef (prog_outputs prog_tt)
        inps     = Set.map convertRef (prog_inputs prog_tt)
        prog_gg' = prog_gg { prog_outputs = outs, prog_inputs = inps }
    return (prog_gg', ctx)

garbleGate :: Ref TruthTable -> Garble (Ref GarbledGate)
garbleGate tt_ref = lookupTT tt_ref >>= \case      -- get the TruthTable
    TTInp i -> do                                  -- if it's an input:
      pair   <- new_wirelabels                     --   get new wirelabels
      gg_ref <- inputp (GarbledInput i)            --   make it a gate, get a ref
      updateContext gg_ref pair                    --   show our work
      return gg_ref                                --   return the gate ref
    tt -> do                                       -- otherwise:
      gg_ref <- nextRef                            --   get a new ref
      let xref = convertRef (tt_inpx tt)           --   get a ref to the left child gate
          yref = convertRef (tt_inpy tt)           --   get a ref to the right child gate
      (gate, out_wl) <- encode gg_ref tt xref yref --   create the garbled table
      writep gg_ref gate                           --   associate ref with garbled gate
      updateContext gg_ref out_wl                  --   show our work
      return gg_ref                                --   return the new gate ref

encode :: Ref GarbledGate -- the ref for this gate (needed for encryption)
       -> TruthTable      -- the TruthTable
       -> Ref GarbledGate -- left child ref
       -> Ref GarbledGate -- right child ref
       -> Garble (GarbledGate, WirelabelPair)
encode ref tt xref yref 
  | isXor tt = do
    x_pair <- pairs_lookup xref
    y_pair <- pairs_lookup yref
    r      <- getR
    let c0 = xorWires (wlp_false x_pair) (wlp_false y_pair)
    return (GarbledXor xref yref, (c0, xorWires c0 r))

  | otherwise = do
    k <- getKey
    x_pair   <- pairs_lookup xref
    y_pair   <- pairs_lookup yref
    out_pair <- new_wirelabels
    let gg_tab = do a <- [True, False]
                    b <- [True, False]
                    let x = sel a x_pair
                        y = sel b y_pair
                        z = sel (tt_f tt a b) out_pair
                        c = enc k ref x y (wl_val z)
                    return ((wl_col x, wl_col y), z { wl_val = c })
    return (GarbledGate xref yref gg_tab, out_pair)

new_wirelabels :: Garble WirelabelPair
new_wirelabels = do
    x <- randBlock
    c <- randBool
    r <- getR
    let w0 = Wirelabel { wl_col = c, wl_val = x }
        w1 = xorWires w0 r
    return (w0, w1)

--------------------------------------------------------------------------------
-- helpers

isXor :: TruthTable -> Bool
isXor tab = show tab == "TT0110"

getKey :: Garble AES
getKey = lift.lift $ gets ctx_key

getR :: Garble Wirelabel
getR = lift.lift $ gets ctx_r

lookupTT :: Ref TruthTable -> Garble TruthTable
lookupTT ref = asks (lookupC ref)

pairs_lookup :: Ref GarbledGate -> Garble WirelabelPair
pairs_lookup ref = lift.lift $ gets (M.lookup ref . ctx_pairs) >>= \case
  Nothing   -> err "pairs_lookup" ("no ref: " ++ show ref)
  Just pair -> return pair

updateContext :: Ref GarbledGate -> WirelabelPair -> Garble ()
updateContext refgg pair = do
  pairs_insert refgg pair
  truth_insert (wlp_true  pair) True
  truth_insert (wlp_false pair) False

pairs_insert :: Ref GarbledGate -> WirelabelPair -> Garble ()
pairs_insert ref pair =
  lift.lift $ modify (\st -> st { ctx_pairs = M.insert ref pair (ctx_pairs st) })

truth_insert :: Wirelabel -> Bool -> Garble ()
truth_insert l b =
  lift.lift $ modify (\st -> st { ctx_truth = M.insert l b (ctx_truth st) })
