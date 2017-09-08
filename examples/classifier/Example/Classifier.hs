module Example.Classifier where

import Crypto.GarbledCircuits
import Crypto.GarbledCircuits.Language as L
import Crypto.GarbledCircuits.Util (bind2, err)

import Control.Monad

classifier :: Program Circuit
classifier = L.buildCircuit $ do
    g_bytes <- replicateM 5 (L.inputs 8 Garbler)
    e_bytes <- replicateM 5 (L.inputs 8 Evaluator)
    comps   <- zipWithM L.gtBinary g_bytes e_bytes
    res     <- L.ands comps
    return [res]
