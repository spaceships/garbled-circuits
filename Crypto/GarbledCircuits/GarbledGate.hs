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
          updateR   =<< randBlock
          -- we assume TT refs are topologically sorted
          mapM_ garbleGate (M.keys (env_deref (prog_env prog_tt)))
        outs     = map (violentLookup $ ctx_refs ctx) (prog_outputs prog_tt)
        inps     = Set.map (violentLookup $ ctx_refs ctx) (prog_inputs prog_tt)
        prog_gg' = prog_gg { prog_outputs = outs, prog_inputs = inps }
    return (prog_gg', ctx)

garbleGate :: Ref TruthTable -> Garble (Ref GarbledGate)
garbleGate tt_ref = lookupTT tt_ref >>= \case            -- get the TruthTable
    TTInp id -> do                                       -- if it's an input:
      pair   <- new_wirelabels                           --   get new wirelabels
      gg_ref <- inputp (GarbledInput id)                 --   make it a gate, get a ref
      updateContext tt_ref gg_ref pair                   --   show our work
      return gg_ref                                      --   return the gate ref
    tt -> do                                             -- otherwise:
      xref <- tt2gg_lookup (tt_inpx tt)                  --   get a ref to the left child gate
      yref <- tt2gg_lookup (tt_inpy tt)                  --   get a ref to the right child gate
      x_wl <- pairs_lookup xref                          --   lookup wirelabels for left child
      y_wl <- pairs_lookup yref                          --   lookup wirelabels for right child
      out_wl <- new_wirelabels                           --   create new wirelabels
      gg_ref <- nextRef                                  --   get a new ref
      table  <- encode gg_ref (tt_f tt) x_wl y_wl out_wl --   create the garbled table
      writep gg_ref (GarbledGate xref yref table)        --   associate ref with garbled gate
      updateContext tt_ref gg_ref out_wl                 --   show our work
      return gg_ref                                      --   return the new gate ref

new_wirelabels :: Garble WirelabelPair
new_wirelabels = do
    x <- randBlock
    y <- randBlock
    c <- randBool
    let wlt = Wirelabel { wl_col = c,     wl_val = x }
        wlf = Wirelabel { wl_col = not c, wl_val = y }
    return $ WirelabelPair { wlp_true = wlt, wlp_false = wlf }

encode :: Ref GarbledGate        -- the ref for this gate (needed for encryption)
       -> (Bool -> Bool -> Bool) -- the function defining a TruthTable
       -> WirelabelPair          -- the x-wirelabel pair
       -> WirelabelPair          -- the y-wirelabel pair
       -> WirelabelPair          -- the out-wirelabel pair
       -> Garble [((Color, Color), Wirelabel)]
encode ref f x_pair y_pair out_pair = do
  k  <- lift.lift $ gets ctx_key
  return $ do -- list monad
    a <- [True, False]
    b <- [True, False]
    let x   = sel a x_pair
        y   = sel b y_pair
        z   = sel (f a b) out_pair
        ct  = enc k ref x y (wl_val z)
        out = z { wl_val = ct }
    return ((wl_col x, wl_col y), out)

--------------------------------------------------------------------------------
-- helpers

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
