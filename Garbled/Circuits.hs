{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Garbled.Circuits where

import Garbled.Circuits.Garbler
import Garbled.Circuits.Types
import Garbled.Circuits.Util (bindM2, err, bits2Word, word2Bits)
import Garbled.Circuits.Plaintext.Language
import Garbled.Circuits.Plaintext.TruthTable

import Control.Monad
import Data.Word

-- circuit language - done
-- intermediate TruthTable representation - done
-- garbled circuit representation - in progress
-- garbling
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
    builder xs ys _ _ = err "builder" "lists of unequal length" [xs,ys]

circ_8BitAdder :: Program Circ
circ_8BitAdder = buildCirc $ do
    inp1      <- replicateM 8 c_input
    inp2      <- replicateM 8 c_input
    (outs, _) <- addBits inp1 inp2
    return outs

eval_8BitAdder :: Word8 -> Word8 -> Word8
eval_8BitAdder x y = bits2Word result
  where
    result = evalCirc circ_8BitAdder (word2Bits x ++ word2Bits y)

-- convert to TruthTable and use TruthTable evaluator
eval_8BitAdderTT :: Word8 -> Word8 -> Word8
eval_8BitAdderTT x y = bits2Word result
  where
    result = evalTT (circ2tt circ_8BitAdder) (word2Bits x ++ word2Bits y)
