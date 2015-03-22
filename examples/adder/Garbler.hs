module Main where

import System.Environment
import Data.Word

import Crypto.GarbledCircuits
import Crypto.GarbledCircuits.Util (word2Bits, bits2Word)

import Example.Adder

main :: IO ()
main = do
    args <- getArgs
    let port  = read (args !! 0)
        input = word2Bits (read (args !! 1) :: Word8)
    result <- listenAt port (garblerProto adder8Bit input . simpleConn)
    print (bits2Word result :: Word8)
