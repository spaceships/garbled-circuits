module Main where

import Data.Word
import System.Environment

import Crypto.GarbledCircuits
import Crypto.GarbledCircuits.Util (word2Bits, bits2Word)

import Example.Adder

main :: IO ()
main = do
    args <- getArgs
    let server = args !! 0
        port   = read (args !! 1)
        input  = word2Bits $ (read (args !! 2) :: Word8)
    result <- evaluatorProto server port adder8Bit input
    print (bits2Word result :: Word8)
