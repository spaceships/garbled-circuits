{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Garbled.Circuits where

import Garbled.Circuits.Garbler
import Garbled.Circuits.Types
import Garbled.Circuits.Util (bindM2, err, bits2Word, word2Bits)
import Garbled.Circuits.Plaintext.Language

import Data.Word
import Control.Monad
import Prelude hiding (and, or)

-- circuit language - done
-- garbled circuit representation - in progress: translate to intermediate TruthTable representation
-- garbling
-- ot
-- evaluation

--------------------------------------------------------------------------------
-- 8 bit adder example

add1Bit :: Ref Circ -> Ref Circ -> Ref Circ -> CircBuilder (Ref Circ, Ref Circ)
add1Bit x y c = do
    s    <- xor x y
    out  <- xor c s
    cout <- bindM2 or (and x y) (and c s)
    return (out, cout)

addBits :: [Ref Circ] -> [Ref Circ] -> CircBuilder ([Ref Circ], Ref Circ)
addBits xs ys = do
    f <- constant False
    builder xs ys f []
  where
    builder [] []         c outs = return (outs, c)
    builder (x:xs) (y:ys) c outs = do
      (out,c') <- add1Bit x y c
      builder xs ys c' (out:outs)
    builder xs ys _ _ = err "builder" "lists of unequal length" [xs,ys]

circ_8BitAdder :: Program Circ
circ_8BitAdder = buildCircuit $ do
    inp1      <- replicateM 8 input
    inp2      <- replicateM 8 input
    (outs, _) <- addBits inp1 inp2
    return outs

eval_8BitAdder :: Word8 -> Word8 -> Word8
eval_8BitAdder x y = bits2Word result
  where
    result = eval circ_8BitAdder (word2Bits x ++ word2Bits y)
