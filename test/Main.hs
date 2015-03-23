module Main where

import Test.GarbledCircuits
import Test.ObliviousTransfer

import Data.Monoid
import Test.Framework

main :: IO ()
main = defaultMainWithOpts tests mempty { ropt_color_mode = Just ColorAlways }

tests = []
  ++ garbledCircuitTests
  ++ obliviousTransferTests
