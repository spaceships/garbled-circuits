module Example.Classifier where

import Crypto.GarbledCircuits
import Crypto.GarbledCircuits.Language as L
import Crypto.GarbledCircuits.Util (bind2, err)

import Control.Monad

classifier :: Program Circuit
classifier = L.buildCircuit $ do
    g_bytes <- replicateM 2 (L.inputs 8 Garbler)
    e_bytes <- replicateM 2 (L.inputs 8 Evaluator)
    comps   <- zipWithM L.gtBinary g_bytes e_bytes
    res     <- L.ands comps
    return [res]

-- to compare two numbers [0-127]
-- return 1 if the first number is bigger, return 0 otherwise
gtBinary :: Program Circuit
gtBinary = L.buildCircuit $ do
  g_bytes <- L.inputs 8 Garbler
  e_bytes <- L.inputs 8 Evaluator
  res <- L.gtBinary g_bytes e_bytes
  return [res]
