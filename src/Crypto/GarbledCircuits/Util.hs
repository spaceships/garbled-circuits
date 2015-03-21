{-# LANGUAGE LambdaCase, ScopedTypeVariables, FlexibleContexts #-}

module Crypto.GarbledCircuits.Util
  ( bind2
  , bits2Word
  , word2Bits
  , evalProg
  , progSize
  , ungarble
  , showWirelabel
  , inputSize
  , inputPairs
  , inputWires
  , nextRef
  , inputp
  , internp
  , writep
  , lookupC
  , lsb
  , sel
  , orBytes
  , xorBytes
  , xorWords
  , mask
  , err
  , (!!!)
  )
where

import Crypto.GarbledCircuits.Types

import           Control.Monad.State
import           Data.Bits hiding (xor)
import qualified Data.Bits
import qualified Data.ByteString  as BS
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import qualified Data.Set as S
import           Data.Tuple
import           Data.Word
import           Numeric (showHex)

--------------------------------------------------------------------------------
-- general helper functions

bind2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bind2 f a b = do x <- a; y <- b; f x y

-- returns a little-endian list of bits
word2Bits :: (FiniteBits b, Num b, Ord b, Bits b) => b -> [Bool]
word2Bits x = map (bitAnd x) (take (finiteBitSize x) pow2s)
  where
    bitAnd a b = a .&. b > 0

-- takes a little-endian list of bits
bits2Word :: (Bits a, Num a) => [Bool] -> a
bits2Word bs = sum $ zipWith select bs pow2s
  where
    select b x = if b then x else 0

pow2s :: (Num b, Bits b) => [b]
pow2s = [ shift 1 x | x <- [0..] ]

xorBytes :: ByteString -> ByteString -> ByteString
xorBytes x y | BS.length x /= BS.length y = err "xor" "unequal length inputs"
             | otherwise = BS.pack $ BS.zipWith Data.Bits.xor x y

orBytes :: ByteString -> ByteString -> ByteString
orBytes x y | BS.length x /= BS.length y = err "xor" "unequal length inputs"
            | otherwise = BS.pack $ BS.zipWith (.|.) x y

xorWords :: [Word8] -> [Word8] -> [Word8]
xorWords = zipWith Data.Bits.xor

progSize :: Program c -> Int
progSize = M.size . prog_env

inputSize :: Party -> Program c -> Int
inputSize p prog = S.size (prog_inputs p prog)

--------------------------------------------------------------------------------
-- garbled gate helpers

lsb :: Wirelabel -> Bool
lsb wl = BS.last wl .&. 1 > 0

sel :: Bool -> WirelabelPair -> Wirelabel
sel b = if b then wlp_true else wlp_false

mask :: Bool -> Wirelabel -> Wirelabel
mask b wl = if b then wl else zeroWirelabel

inputPairs :: Party -> Program GarbledGate -> Context -> [WirelabelPair]
inputPairs p prog ctx = map (ctx_pairs ctx !!!) (S.toList (prog_inputs p prog))

inputWires :: Party -> Program GarbledGate -> Context -> [Bool] -> [Wirelabel]
inputWires party prog ctx inp = zipWith sel inp (inputPairs party prog ctx)

ungarble :: Context -> Wirelabel -> Bool
ungarble ctx wl = case M.lookup wl (ctx_truth ctx) of
  Nothing -> err "ungarble" $ "unknown wirelabel: " ++ showWirelabel wl
  Just b  -> b

showWirelabel :: Wirelabel -> String
showWirelabel wl = "wl" ++ showCol (lsb wl) ++ " " ++ hexStr
    where showCol b = if b then "1" else "0"
          hexStr = concatMap (pad . hex) $ BS.unpack wl
          pad s = if length s == 1 then '0' : s else s
          hex = flip showHex ""

--------------------------------------------------------------------------------
-- polymorphic helper functions for State monads over a Program

nextRef :: (Ord c, MonadState (Program c) m) => m (Ref c)
nextRef = do
  env <- gets prog_env
  return $ succ (fst (M.findMax env))

internp :: (Ord c, MonadState (Program c) m) => c -> m (Ref c)
internp circ = do
  prog <- get
  let env   = prog_env prog
      dedup = map swap (M.toList env)
  case lookup circ dedup of
    Just ref -> return ref
    Nothing  -> do
      let ref  = if M.null env then Ref 0 else succ $ fst (M.findMax env)
          env' = M.insert ref circ env
      put prog { prog_env = env' }
      return ref

inputp :: (Ord c, MonadState (Program c) m) => Party -> c -> m (Ref c)
inputp party inp = do
  ref <- internp inp
  modify $ \p -> case party of
    PartyA -> p { prog_input_a = S.insert ref (prog_input_a p) }
    PartyB -> p { prog_input_b = S.insert ref (prog_input_b p) }
  return ref

writep :: (Ord c, MonadState (Program c) m) => Ref c -> c -> m ()
writep ref circ = modify (\p -> p { prog_env = M.insert ref circ (prog_env p) })

lookupC :: Ref c -> Program c -> c
lookupC ref prog = fromMaybe (error "[lookupC] no c") (M.lookup ref (prog_env prog))

--------------------------------------------------------------------------------
-- polymorphic evaluation

evalProg :: (Show b, CanHaveChildren c)
         => (Ref c -> c -> [b] -> b) -> Program c -> [b]
evalProg construct prog = outputs
  where
    resultMap = execState (traverse construct prog) M.empty
    outputs   = map (resultMap !!!) (prog_output prog)

traverse :: (Show b, MonadState (Map (Ref c) b) m, CanHaveChildren c)
         => (Ref c -> c -> [b] -> b) -> Program c -> m ()
traverse construct prog = mapM_ eval (M.keys (prog_env prog))
  where
    getVal ref = get >>= \precomputed ->
      case M.lookup ref precomputed of
        Nothing  -> err "traverse.getVal" ("unknown ref: " ++ show ref)
        Just res -> return res
    eval ref = do
      let c = lookupC ref prog
      kids <- mapM getVal (children c)
      let result = construct ref c kids
      modify (M.insert ref result)
      return result

--------------------------------------------------------------------------------
-- evil helpers

err :: String -> String -> a
err name warning = error $ "[" ++ name ++ "] " ++ warning

(!!!) :: (Show k, Show v, Ord k) => Map k v -> k -> v
m !!! k = case M.lookup k m of
  Nothing -> err "!!!" ("OOPS: " ++ show m)
  Just  v -> v
