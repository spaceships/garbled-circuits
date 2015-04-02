module Main where

import Prelude hiding (and)
import System.Environment

import Crypto.GarbledCircuits
import Crypto.GarbledCircuits.Language

bitAnd :: Program Circuit
bitAnd = buildCircuit $ do
    x <- input Garbler
    y <- input Evaluator
    z <- and x y
    return [z]

main :: IO ()
main = do
    args <- getArgs
    let server = args !! 0
        port   = read (args !! 1)
        inp    = read (args !! 2) :: Bool
        proto  = evaluatorProto bitAnd [inp] . simpleConn
    result <- connectTo server port proto
    print result
