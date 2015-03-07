{-# LANGUAGE LambdaCase, NamedFieldPuns #-}

module Crypto.GarbledCircuits.GarbledGate
  (
    tt2gg
  , garble
  )
where

import Crypto.GarbledCircuits.Encryption
import Crypto.GarbledCircuits.TruthTable
import Crypto.GarbledCircuits.Types
import Crypto.GarbledCircuits.Util

import           Control.Monad.Reader
import           Control.Monad.State
import           Crypto.Cipher.AES
import           Crypto.Random
import           Data.Functor
import qualified Data.Map         as M
import qualified Data.Set         as Set

--------------------------------------------------------------------------------
-- TODO: add optimizations: point-and-permute, free-xor, row-reduction, half-gates

--------------------------------------------------------------------------------
-- garble a truthtable program

runGarble :: SystemRNG -> Program TruthTable -> Garble a -> (Program GarbledGate, Context)
runGarble gen prog_tt = 
    flip runState emptyContext
  . flip runReaderT prog_tt
  . flip evalStateT gen
  . flip execStateT emptyProg

garble :: Program Circ -> IO (Program GarbledGate, Context)
garble = tt2gg . circ2tt

tt2gg :: Program TruthTable -> IO (Program GarbledGate, Context)
tt2gg prog_tt = do
    gen <- cprgCreate <$> createEntropyPool
    let (prog_gg, ctx) = runGarble gen prog_tt $ do
          updateKey =<< genKey
          updateR   =<< genR
          -- we assume TT refs are topologically sorted
          mapM_ garbleGate (M.keys (env_deref (prog_env prog_tt)))
        outs     = map (violentLookup $ ctx_refs ctx) (prog_outputs prog_tt)
        inps     = Set.map (violentLookup $ ctx_refs ctx) (prog_inputs prog_tt)
        prog_gg' = prog_gg { prog_outputs = outs, prog_inputs = inps }
    return (prog_gg', ctx)

garbleGate :: Ref TruthTable -> Garble (Ref GarbledGate)
garbleGate tt_ref = lookupTT tt_ref >>= \case            -- get the TruthTable
    TTInp id -> do                                       -- if it's an input:
      pair   <- input_wirelabels                         --   get new wirelabels
      gg_ref <- inputp (GarbledInput id)                 --   make it a gate, get a ref
      updateContext tt_ref gg_ref pair                   --   show our work
      return gg_ref                                      --   return the gate ref
    tt -> do                                             -- otherwise:
      xref <- tt2gg_lookup (tt_inpx tt)                  --   get a ref to the left child gate
      yref <- tt2gg_lookup (tt_inpy tt)                  --   get a ref to the right child gate
      gg_ref <- nextRef                                  --   get a new ref
      (gate, out_wl) <- encode gg_ref tt xref yref       --   create the garbled table
      writep gg_ref gate                                 --   associate ref with garbled gate
      updateContext tt_ref gg_ref out_wl                 --   show our work
      return gg_ref                                      --   return the new gate ref

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
                        z = sel ((tt_f tt) a b) out_pair
                        c = enc k ref x y (wl_val z)
                    return ((wl_col x, wl_col y), z { wl_val = c })
    return (GarbledGate xref yref gg_tab, out_pair)

input_wirelabels :: Garble WirelabelPair
input_wirelabels = do
    x <- randBlock
    c <- randBool
    r <- getR
    let w0 = Wirelabel { wl_col = c, wl_val = x }
        w1 = xorWires w0 r
    return (w0, w1)

new_wirelabels :: Garble WirelabelPair
new_wirelabels = do
    x <- randBlock
    y <- randBlock
    c <- randBool
    let wlt = Wirelabel { wl_col = c,     wl_val = x }
        wlf = Wirelabel { wl_col = not c, wl_val = y }
    return (wlf, wlt)

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

tt2gg_lookup :: Ref TruthTable -> Garble (Ref GarbledGate)
tt2gg_lookup ref = do
    res <- lift.lift $ gets (M.lookup ref . ctx_refs)
    case res of
      Nothing  -> err "tt2gg_lookup" ("ref " ++ show ref ++ " has not been garbled")
      Just ref -> return ref

pairs_lookup :: Ref GarbledGate -> Garble WirelabelPair
pairs_lookup ref = lift.lift $ gets (M.lookup ref . ctx_pairs) >>= \case
  Nothing   -> err "pairs_lookup" ("no ref: " ++ show ref)
  Just pair -> return pair

updateContext :: Ref TruthTable -> Ref GarbledGate -> WirelabelPair -> Garble ()
updateContext reftt refgg pair = do
  tt2gg_insert reftt refgg
  pairs_insert refgg pair
  truth_insert (wlp_true  pair) True
  truth_insert (wlp_false pair) False

tt2gg_insert :: Ref TruthTable -> Ref GarbledGate -> Garble ()
tt2gg_insert x y =
  lift.lift $ modify (\st -> st { ctx_refs = M.insert x y (ctx_refs st) })

pairs_insert :: Ref GarbledGate -> WirelabelPair -> Garble ()
pairs_insert ref pair =
  lift.lift $ modify (\st -> st { ctx_pairs = M.insert ref pair (ctx_pairs st) })

truth_insert :: Wirelabel -> Bool -> Garble ()
truth_insert l b =
  lift.lift $ modify (\st -> st { ctx_truth = M.insert l b (ctx_truth st) })
