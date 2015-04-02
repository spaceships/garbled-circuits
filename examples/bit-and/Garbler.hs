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
    let port  = read (args !! 0)
        inp   = read (args !! 1) :: Bool
        proto = garblerProto bitAnd [inp] . simpleConn
    result <- listenAt port proto
    print result
