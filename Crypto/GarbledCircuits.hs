{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Crypto.GarbledCircuits where

import Crypto.GarbledCircuits.Types
import Crypto.GarbledCircuits.Util (violentLookup, bindM2, err, bits2Word, word2Bits)
import Crypto.GarbledCircuits.Language
import Crypto.GarbledCircuits.TruthTable
import Crypto.GarbledCircuits.GarbledGate

import Control.Applicative
import Control.Monad
import Data.Word

-- circuit language - done
-- intermediate TruthTable representation - done
-- garbled circuit representation - done
-- garbling - in progress - add optimizations (free xor, row reduction, half gates)
-- ot
-- evaluation

--------------------------------------------------------------------------------
-- 8 bit adder example

add1Bit :: Ref Circ -> Ref Circ -> Ref Circ -> CircBuilder (Ref Circ, Ref Circ)
add1Bit x y c = do
    s    <- c_xor x y
    out  <- c_xor c s
    cout <- bindM2 c_or (c_and x y) (c_and c s)
    return (out, cout)

addBits :: [Ref Circ] -> [Ref Circ] -> CircBuilder ([Ref Circ], Ref Circ)
addBits xs ys = do
    f <- c_const False
    builder xs ys f []
  where
    builder [] []         c outs = return (outs, c)
    builder (x:xs) (y:ys) c outs = do
      (out,c') <- add1Bit x y c
      builder xs ys c' (out:outs)
    builder xs ys _ _ = err "builder" ("lists of unequal length: " ++ show [xs,ys])

circ_NBitAdder :: Int -> Program Circ
circ_NBitAdder n = buildCirc $ do
    inp1      <- replicateM n c_input
    inp2      <- replicateM n c_input
    (outs, _) <- addBits inp1 inp2
    return outs

circ_8BitAdder :: Program Circ
circ_8BitAdder = circ_NBitAdder 8

eval_2BitAdder :: (Bool, Bool) -> (Bool, Bool) -> IO [Bool]
eval_2BitAdder (x0,x1) (y0,y1) = evalCirc (circ_NBitAdder 2) [x0,x1,y0,y1]

eval_2BitAdderGG :: (Bool, Bool) -> (Bool, Bool) -> IO [Bool]
eval_2BitAdderGG (x0,x1) (y0,y1) = do
  gg <- garble (circ_NBitAdder 2)
  evalGG [x0,x1,y0,y1] gg

eval_8BitAdder :: Word8 -> Word8 -> IO Word8
eval_8BitAdder x y = bits2Word <$> result
  where
    result = evalCirc circ_8BitAdder (word2Bits x ++ word2Bits y)

-- convert to TruthTable and use TruthTable evaluator
eval_8BitAdderTT :: Word8 -> Word8 -> IO Word8
eval_8BitAdderTT x y = bits2Word <$> result
  where
    result = evalTT (circ2tt circ_8BitAdder) (word2Bits x ++ word2Bits y)

-- convert to GarbledGate and use GG evaluator
eval_8BitAdderGG :: Word8 -> Word8 -> IO Word8
eval_8BitAdderGG x y = do
    gg <- garble circ_8BitAdder
    result <- evalGG (word2Bits x ++ word2Bits y) gg
    return (bits2Word result)