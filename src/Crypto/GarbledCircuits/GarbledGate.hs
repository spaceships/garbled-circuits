{-# LANGUAGE PackageImports, LambdaCase, NamedFieldPuns #-}

module Crypto.GarbledCircuits.GarbledGate
  ( garble
  , halfGates
  , newWirelabels
  , reconstruct
  , runGarble
  , runGarble'
  , tt2gg
  )
where

import Crypto.GarbledCircuits.Encryption
import Crypto.GarbledCircuits.TruthTable
import Crypto.GarbledCircuits.Types
import Crypto.GarbledCircuits.Util

import           Control.Monad.Reader
import           Control.Monad.State
import           Crypto.Cipher.AES128
import qualified Data.Bits
import           Data.Functor
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Tuple (swap)

--------------------------------------------------------------------------------
-- garble a truthtable program

garble :: Program Circuit -> IO (Program GarbledGate, Context)
garble = tt2gg . circ2tt

runGarble :: Program TruthTable -> Garble a -> IO (Program GarbledGate, Context)
runGarble prog_tt g = do
    ((_, p), c) <- runGarble' prog_tt g 
    return (p, c)

runGarble' :: Program TruthTable -> Garble a -> IO ((a, Program GarbledGate), Context)
runGarble' prog_tt =
      flip runStateT emptyContext
    . flip runReaderT prog_tt
    . flip runStateT emptyProg

tt2gg :: Program TruthTable -> IO (Program GarbledGate, Context)
tt2gg prog_tt = do
    (prog_gg, ctx) <- runGarble prog_tt $ do
      updateKey =<< genKey
      updateR   =<< genR
      -- TT refs are topologically ordered
      mapM_ garbleGate (M.keys (prog_env prog_tt))
    let outs     = map convertRef (prog_output prog_tt)
        prog_gg' = prog_gg { prog_output = outs }
    return (prog_gg', ctx)

garbleGate :: Ref TruthTable -> Garble (Ref GarbledGate)
garbleGate tt_ref = lookupTT tt_ref >>= \case -- get the TruthTable
    TTInp i p -> do                           -- if it's an input:
      pair   <- newWirelabels                 --   get new wirelabels
      gg_ref <- inputp p (GarbledInput i p)   --   make it a gate, get a ref
      updateContext gg_ref pair               --   show our work
      return gg_ref                           --   return the gate ref
    tt -> do                                  -- otherwise:
      gg_ref <- nextRef                       --   get a new ref
      let xref = convertRef (tt_inpx tt)      --   get a ref to the left child gate
          yref = convertRef (tt_inpy tt)      --   get a ref to the right child gate
      (gate, out_wl) <- encode tt xref yref   --   create the garbled table
      writep gg_ref gate                      --   associate ref with garbled gate
      updateContext gg_ref out_wl             --   show our work
      return gg_ref                           --   return the new gate ref

encode :: TruthTable      -- the TruthTable
       -> Ref GarbledGate -- left child ref
       -> Ref GarbledGate -- right child ref
       -> Garble (GarbledGate, WirelabelPair)
encode tt aref bref
  | isXor tt = do
    a_pair <- maybeFlipWires (tt_negx tt) <$> pairsLookup aref
    b_pair <- maybeFlipWires (tt_negy tt) <$> pairsLookup bref
    r      <- getR
    let c0 = wlp_false a_pair `xorBytes` wlp_false b_pair
    return (FreeXor aref bref, (c0, c0 `xorBytes` r))

  | halfGateCompatible tt = do -- the truth table is half-gate compatible
    let (fg, a, b) = get_fg (tt_f tt)
    k <- getKey
    r <- getR
    a_pair <- maybeFlipWires (tt_negx tt) <$> pairsLookup aref
    b_pair <- maybeFlipWires (tt_negy tt) <$> pairsLookup bref
    let pa = lsb (wlp_false a_pair)
        pb = lsb (wlp_false b_pair)
    j  <- nextIndex
    j' <- nextIndex
    let g  = hash k (wlp_false a_pair) j  `xorBytes` hash k (wlp_true a_pair)  j 
                                          `xorBytes` mask (Data.Bits.xor pb b) r
        wg = hash k (sel pa a_pair) j     `xorBytes` mask (fg pa pb) r
        e  = hash k (wlp_false b_pair) j' `xorBytes` hash k (wlp_true b_pair)  j' 
                                          `xorBytes` sel a a_pair
        we = hash k (sel pb b_pair) j'
        w  = wg `xorBytes` we
    return (HalfGate aref bref g e, (w, w `xorBytes` r))

  | otherwise = err "encode" ("unsupported gate: " ++ show tt)

newWirelabels :: Garble WirelabelPair
newWirelabels = do
    a <- randBlock
    r <- getR
    return (a, a `xorBytes` r)

halfGates :: Program GarbledGate -> [(Wirelabel, Wirelabel)]
halfGates = map vals . filter halfGate . map snd . M.toList . prog_env
  where
    halfGate (HalfGate _ _ _ _) = True
    halfGate _                  = False
    vals (HalfGate _ _ g e)     = (g,e)
    vals _                      = err "halfGates" "not half gate"

-- create a Program GarbledGate given a TruthTable and a list of HalfGates
reconstruct :: Program TruthTable -> [(Wirelabel, Wirelabel)] -> Program GarbledGate
reconstruct prog hgs = Program { prog_input_a = S.map convertRef (prog_input_a prog)
                               , prog_input_b = S.map convertRef (prog_input_b prog)
                               , prog_output  = map convertRef (prog_output prog)
                               , prog_env     = M.fromList (doStuff (M.toList (prog_env prog)) hgs)
                               }

doStuff :: [(Ref TruthTable, TruthTable)] 
        -> [(Wirelabel, Wirelabel)] 
        -> [(Ref GarbledGate, GarbledGate)]
doStuff []              _  = []
doStuff ((ref,tt):rest) hs = 
  let (gg,hs') = case tt of {
    TTInp i p -> (GarbledInput i p, hs);
    _         -> case tt_f tt of
      XOR   -> (FreeXor  (convertRef (tt_inpx tt)) (convertRef (tt_inpy tt)), hs);
      _     -> (HalfGate (convertRef (tt_inpx tt)) (convertRef (tt_inpy tt)) (fst (head hs)) (snd (head hs)), tail hs);
  }
  in (convertRef ref, gg) : doStuff rest hs'

--------------------------------------------------------------------------------
-- helpers

maybeFlipWires :: Bool -> WirelabelPair -> WirelabelPair
maybeFlipWires True  = swap
maybeFlipWires False = id

isXor :: TruthTable -> Bool
isXor tt = tt_f tt == XOR

halfGateCompatible :: TruthTable -> Bool
halfGateCompatible tt = tt_f tt == AND || tt_f tt == OR

get_fg :: Operation -> (Bool -> Bool -> Bool, Bool, Bool)
get_fg op = case op of
    AND -> (fg False False False, False, False)
    OR  -> (fg True True True, True, True)
    _   -> err "get_fg" ("unsupported op: " ++ show op)
  where
    fg a b c x y = Data.Bits.xor (Data.Bits.xor a x && Data.Bits.xor b y) c

nextIndex :: Garble Int
nextIndex = do
    c <- lift.lift $ get
    let ctr = ctx_ctr c
    lift.lift $ put c { ctx_ctr = succ ctr }
    return ctr

getKey :: Garble AESKey128
getKey = lift.lift $ gets ctx_key

getR :: Garble Wirelabel
getR = lift.lift $ gets ctx_r

lookupTT :: Ref TruthTable -> Garble TruthTable
lookupTT ref = asks (lookupC ref)

pairsLookup :: Ref GarbledGate -> Garble WirelabelPair
pairsLookup ref = lift.lift $ gets (M.lookup ref . ctx_pairs) >>= \case
  Nothing   -> err "pairsLookup" ("no ref: " ++ show ref)
  Just pair -> return pair

updateContext :: Ref GarbledGate -> WirelabelPair -> Garble ()
updateContext refgg pair = do
  pairsInsert refgg pair
  truthInsert (wlp_true  pair) True
  truthInsert (wlp_false pair) False

pairsInsert :: Ref GarbledGate -> WirelabelPair -> Garble ()
pairsInsert ref pair =
  lift.lift $ modify (\st -> st { ctx_pairs = M.insert ref pair (ctx_pairs st) })

truthInsert :: Wirelabel -> Bool -> Garble ()
truthInsert l b =
  lift.lift $ modify (\st -> st { ctx_truth = M.insert l b (ctx_truth st) })
