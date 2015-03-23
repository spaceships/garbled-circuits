module Main where

import TestGarbledCircuits
import TestObliviousTransfer

import Data.Monoid
import Test.Framework

main :: IO ()
main = defaultMainWithOpts tests mempty { ropt_color_mode = Just ColorAlways }

tests = []
  ++ garbledCircuitTests
  ++ obliviousTransferTests
