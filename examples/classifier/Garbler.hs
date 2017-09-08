module Main where

import System.Environment
import Data.Word

import Crypto.GarbledCircuits
import Crypto.GarbledCircuits.Util (word2Bits, bits2Word)

import Example.Classifier

main :: IO ()
main = do
    (port:args) <- getArgs
    let input = concat [ word2Bits (read w :: Word8) | w <- args ]
    result <- listenAt (read port) (garblerProto classifier input . simpleConn)
    print (bits2Word result :: Word8)
