module Main where

import Control.Monad
import Data.Word
import System.Environment

import Crypto.GarbledCircuits
import Crypto.GarbledCircuits.Util (word2Bits, bits2Word)

import Example.Classifier

main :: IO ()
main = do
    (server:port:args) <- getArgs
    let input = concat [ word2Bits (read w :: Word8) | w <- args ]
    result <- connectTo server (read port) (evaluatorProto gtBinary input <=< simpleConn)
    print (bits2Word result :: Word8)
