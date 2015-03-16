module Main where

import Data.Word
import System.Environment

import Crypto.GarbledCircuits
import Crypto.GarbledCircuits.Util (word2Bits, bits2Word)

main :: IO ()
main = do
    args <- getArgs
    let server = read (args !! 0)
        port   = read (args !! 1)
        input  = word2Bits $ (read (args !! 2) :: Word8)
    result <- evaluatorProto server port input
    print (bits2Word result :: Word8)
