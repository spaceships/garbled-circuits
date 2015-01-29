{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Garbled.Circuits where

import Garbled.Circuits.Plaintext.Language
import Garbled.Circuits.Plaintext.Types
import Garbled.Circuits.Garbler
import Garbled.Circuits.Util

import Prelude hiding (and, or)
import Control.Monad

-- circuit language - done
-- garbled circuit representation
-- garbling
-- ot
-- evaluation

--------------------------------------------------------------------------------
-- 8 bit adder example

add1Bit :: CircRef -> CircRef -> CircRef -> CircuitBuilder (CircRef, CircRef)
add1Bit x y c = do
    s    <- xor x y
    out  <- xor c s
    cout <- bindM2 or (and x y) (and c s)
    return (out, cout)

addBits :: [CircRef] -> [CircRef] -> CircuitBuilder ([CircRef], CircRef)
addBits xs ys = do
    f <- constant False
    builder xs ys f []
  where
    builder [] []         c outs = return (outs, c)
    builder (x:xs) (y:ys) c outs = do 
      (out,c') <- add1Bit x y c
      builder xs ys c' (out:outs)

circ_8BitAdder :: Program
circ_8BitAdder = buildCircuit $ do
    inp1      <- replicateM 8 input
    inp2      <- replicateM 8 input
    (outs, _) <- addBits inp1 inp2
    return outs
