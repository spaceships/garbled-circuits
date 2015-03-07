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

import           Control.Monad.Reader
import           Control.Monad.State
import           Crypto.Cipher.AES
import           Crypto.Random
import           Data.Bits
import qualified Data.ByteString as B
import           Data.Functor
import qualified Data.Map as M
import qualified Data.Serialize as S
import qualified Data.Set as S
import           Data.Word

--------------------------------------------------------------------------------
-- TODO: add optimizations: point-and-permute, free-xor, row-reduction, half-gates

--------------------------------------------------------------------------------
-- security parameter: determines size of keys and wirelabels

sec = Crypto.GarbledCircuits.Types.securityParameter

--------------------------------------------------------------------------------
-- data types for garbling

type Garble = StateT (Program GarbledGate)
                (StateT SystemRNG
                  (ReaderT (Program TruthTable)
                    (State Context)))

data Context = Context { things_refs  :: Map (Ref TruthTable) (Ref GarbledGate)
                       , things_pairs :: Map (Ref GarbledGate) WirelabelPair
                       , things_truth :: Map Wirelabel Bool
                       , things_key   :: AES
                       }

--------------------------------------------------------------------------------
-- encryption and decryption for wirelabels

-- AES-based garbling. Uses native hw instructions if available. Source:
-- https://web.engr.oregonstate.edu/~rosulekm/scbib/index.php?n=Paper.BHKR13
-- garbling: pi(K || T) xor K xor M where K = 2A xor 4B
--           where tweak = gateNum ++ colorX ++ colorY
--                 pi is publicly keyed block cipher (AES)
enc :: AES -> Ref GarbledGate -> Wirelabel -> Wirelabel -> Ciphertext -> Ciphertext
enc key gateRef x y z = encryptECB key (B.append k tweak) `xor` k `xor` z
  where
    k     = double (wl_val x) `xor` double (double (wl_val y))
    tweak = S.encode (unRef gateRef, bit (wl_col x), bit (wl_col y))

    bit :: Bool -> Word32
    bit b = if b then 1 else 0

    xor :: Ciphertext -> Ciphertext -> Ciphertext
    xor x y = B.pack $ B.zipWith Data.Bits.xor x y

    xor' :: [Word8] -> [Word8] -> [Word8]
    xor' x y = zipWith Data.Bits.xor x y

    double :: Ciphertext -> Ciphertext
    double c = B.pack result
      where
        (xs, carry) = shiftLeft (B.unpack c)
        result      = if carry > 0 then xor' xs irreducible else xs

    irreducible :: [Word8]
    irreducible = replicate 15 0 ++ [86]

    shiftLeft :: [Word8] -> ([Word8], Word8)
    shiftLeft []     = ([], 0)
    shiftLeft (x:xs) = let (xs', c) = shiftLeft xs
                           msb = shiftR x 7
                           x'  = shiftL x 1 .|. c
                       in (x':xs', msb)

dec :: AES -> Ref GarbledGate -> Wirelabel -> Wirelabel -> Ciphertext -> Ciphertext
dec = enc

--------------------------------------------------------------------------------
-- garbling

garble :: Program Circ -> IO (Program GarbledGate, Context)
garble = tt2gg . circ2tt

tt2gg :: Program TruthTable -> IO (Program GarbledGate, Context)
tt2gg prog_tt = do
    gen <- cprgCreate <$> createEntropyPool
    let (prog_gg, things) = runGarble gen $ do
          updateKey =<< genKey
          -- we assume TT refs are topologically sorted
          mapM_ garbleGate (M.keys (env_deref (prog_env prog_tt)))
        outs     = map (violentLookup $ things_refs things) (prog_outputs prog_tt)
        inps     = S.map (violentLookup $ things_refs things) (prog_inputs prog_tt)
        prog_gg' = prog_gg { prog_outputs = outs, prog_inputs = inps }
    return (prog_gg', things)
  where
    runGarble :: SystemRNG -> Garble a -> (Program GarbledGate, Context)
    runGarble gen = flip runState (Context M.empty M.empty M.empty undefined)
                  . flip runReaderT prog_tt
                  . flip evalStateT gen
                  . flip execStateT emptyProg

garbleGate :: Ref TruthTable -> Garble (Ref GarbledGate)
garbleGate tt_ref = lookupTT tt_ref >>= \case            -- get the TruthTable
    TTInp id -> do                                       -- if it's an input:
      pair   <- new_wirelabels                           --   get new wirelabels
      gg_ref <- inputp (GarbledInput id)                 --   make it a gate, get a ref
      allthethings tt_ref gg_ref pair                    --   show our work in the state
      return gg_ref                                      --   return the gate ref
    tt -> do                                             -- otherwise:
      xref <- tt2gg_lookup (tt_inpx tt)                  --   get a ref for the left child gate
      yref <- tt2gg_lookup (tt_inpy tt)                  --   get a ref for the right child gate
      x_wl <- pairs_lookup xref                          --   lookup wirelabels for left child
      y_wl <- pairs_lookup yref                          --   lookup wirelabels for right child
      out_wl <- new_wirelabels                           --   get new wirelabels
      gg_ref <- nextRef                                  --   get a new ref
      table  <- encode gg_ref (tt_f tt) x_wl y_wl out_wl --   create the garbled table
      writep gg_ref (GarbledGate xref yref table)        --   add garbled table to the Prog
      allthethings tt_ref gg_ref out_wl                  --   show our work in the state
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
  k  <- lift.lift $ gets things_key
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
-- evaluator

evalGG :: [Bool] -> (Program GarbledGate, Context) -> IO [Bool]
evalGG inps (prog, things) = do
    result <- evalProg reconstruct prog inpwires :: IO [Wirelabel]
#ifdef DEBUG
    let out_pairs  = map (\ref -> (ref, violentLookup (things_pairs things) ref)) (prog_outputs prog)
        out_truths = map (\(ref, pair) -> (ref, wlp_true pair, wlp_false pair)) out_pairs
    forM out_truths $ \(ref, t, f) -> do
      putStrLn ("[evalProg] outwire " ++ show ref ++ " true:  " ++ show t)
      putStrLn ("[evalProg] outwire " ++ show ref ++ " false: " ++ show f)
#endif
    return (map ungarble result)
  where
    k = things_key things

    inpwlps  = map (violentLookup $ things_pairs things) (S.toList $ prog_inputs prog)
    inpwires = zipWith sel inps inpwlps
    inputs   = zip (map InputId [0..]) inpwires

    reconstruct :: Ref GarbledGate -> GarbledGate -> [Wirelabel] -> IO Wirelabel
    reconstruct _ (GarbledInput id) [] = case lookup id inputs of
      Nothing -> err "reconstruct" ("no input wire with id " ++ show id ++ "\n" ++ show inputs)
      Just wl -> return wl
    reconstruct ref g [x,y] = case lookup (wl_col x, wl_col y) (gate_table g) of
      Nothing -> err "reconstruct" "no matching color"
      Just z  -> do
        let new_val = dec k ref x y (wl_val z)
            new_wl  = z { wl_val = new_val }
        return new_wl
    reconstruct _ _ _ = err "reconstruct" "unknown pattern"

    ungarble :: Wirelabel -> Bool
    ungarble wl = case M.lookup wl (things_truth things) of
      Nothing -> err "ungarble" $ "unknown wirelabel: " ++ show wl
#ifdef DEBUG
                                  ++ "\n" ++ showEnv prog ++ showPairs things
#endif
      Just b  -> b

--------------------------------------------------------------------------------
-- helpers

genKey :: Garble AES
genKey = initAES <$> randBlock

randBlock :: Garble Ciphertext
randBlock = do
  gen <- lift get
  let (blk, gen') = cprgGenerate sec gen
  lift $ put gen'
  return blk

randBool :: Garble Bool
randBool = do
  gen <- lift get
  let (blk, gen') = cprgGenerate 1 gen
      w8 = head (B.unpack blk)
  lift $ put gen'
  return (w8 .&. 1 > 0)

updateKey :: AES -> Garble ()
updateKey k = lift.lift $ modify (\st -> st { things_key = k })

lookupTT :: Ref TruthTable -> Garble TruthTable
lookupTT ref = asks (lookupC ref)

tt2gg_lookup :: Ref TruthTable -> Garble (Ref GarbledGate)
tt2gg_lookup ref = do
    res <- lift.lift $ gets (M.lookup ref . things_refs)
    case res of
      Nothing  -> err "tt2gg_lookup" ("ref " ++ show ref ++ " has not been garbled")
      Just ref -> return ref

pairs_lookup :: Ref GarbledGate -> Garble WirelabelPair
pairs_lookup ref = lift.lift $ gets (M.lookup ref . things_pairs) >>= \case
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
  lift.lift $ modify (\st -> st { things_refs = M.insert x y (things_refs st) })

pairs_insert :: Ref GarbledGate -> WirelabelPair -> Garble ()
pairs_insert ref pair =
  lift.lift $ modify (\st -> st { things_pairs = M.insert ref pair (things_pairs st) })

truth_insert :: Wirelabel -> Bool -> Garble ()
truth_insert l b =
  lift.lift $ modify (\st -> st { things_truth = M.insert l b (things_truth st) })

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

showPairs :: Context -> String
showPairs things =
    "--------------------------------------------------------------------------------\n"
    ++ "-- pairs \n" ++ concatMap showPair (M.toList (things_pairs things))
  where
    showPair (ref, pair) = show ref ++ ": true=" ++ show (wlp_true pair)
                                    ++ " false=" ++ show (wlp_false pair) ++ "\n"
