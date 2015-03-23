{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.GarbledCircuits where

import Control.Concurrent
import Data.Functor
import Data.Maybe
import Data.Serialize

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Crypto.GarbledCircuits
import qualified Crypto.GarbledCircuits.Language as L
import Crypto.GarbledCircuits.GarbledGate
import Crypto.GarbledCircuits.TruthTable
import Crypto.GarbledCircuits.Encryption
import Crypto.GarbledCircuits.Eval
import Crypto.GarbledCircuits.Types
import Crypto.GarbledCircuits.Util

garbledCircuitTests :: [Test]
garbledCircuitTests = [ testProperty "The colors of new wirelabels are different" prop_colorsDifferent
                      , testProperty "lsb R always equals 1" prop_lsbOfR
                      , testProperty "Arbitrary circuit is correct" prop_arbitraryCircuitCorrect
                      , testProperty "Reconstruct is correct" prop_reconstructCorrect
                      , testProperty "Serialization is correct" prop_serializeCorrect
                      , testProperty "Protocol works" prop_protoWorks
                      ]

prop_colorsDifferent :: Property
prop_colorsDifferent = monadicIO $ do
    p <- testGarble newWirelabels
    assert $ lsb (wlp_true p) /= lsb (wlp_false p)

prop_lsbOfR :: Property
prop_lsbOfR = monadicIO (lsb <$> testGarble genR)

prop_arbitraryCircuitCorrect :: Program Circuit -> Property
prop_arbitraryCircuitCorrect circ = monadicIO $ do
    (_,gg,ctx) <- testCircuit circ
    inpA <- pick $ vector (inputSize Garbler   circ)
    inpB <- pick $ vector (inputSize Evaluator circ)
    let pt  = L.evalCircuit  inpA inpB circ
        res = evalLocal inpA inpB (gg,ctx)
    assert (res == pt)

ensureEither :: Eq b => Either a b -> b -> Bool
ensureEither e eq = either (const False) (== eq) e

prop_reconstructCorrect :: Program Circuit -> Property
prop_reconstructCorrect circ = monadicIO $ do
    (tt, gg, _) <- testCircuit circ
    assert $ reconstruct tt (halfGates gg) == gg

prop_serializeCorrect :: Program Circuit -> Property
prop_serializeCorrect circ = monadicIO $ do
    (tt, gg, _) <- testCircuit circ
    assert $ ensureEither (reconstruct tt <$> decode (encode (halfGates gg))) gg

prop_protoWorks :: Program Circuit -> Property
prop_protoWorks prog = once $ monadicIO $ do
    inpA <- pick $ vector (inputSize Garbler   prog)
    inpB <- pick $ vector (inputSize Evaluator prog)
    let pt = L.evalCircuit inpA inpB prog
    chan <- run newChan
    port <- run $ generate (choose (1024,65536)) 
    run $ forkIO $ do
      res <- listenAt port (garblerProto prog inpA . simpleConn)
      writeChan chan res
    ggEval <- run $ connectTo "localhost" port (evaluatorProto prog inpB . simpleConn)
    ggGarb <- run $ readChan chan
    assert (ggEval == pt && ggGarb == pt)

--------------------------------------------------------------------------------
-- helpers

testCircuit :: Program Circuit -> PropertyM IO (Program TruthTable, Program GarbledGate, Context)
testCircuit circ = do
    let tt = circ2tt circ
    (gg, ctx) <- run (tt2gg tt)
    return (tt, gg, ctx)

testGarble :: Garble a -> PropertyM IO a
testGarble g = do
    ((x, _), _) <- run $ runGarble' emptyProg $ do
      updateKey =<< genKey
      updateR   =<< genR
      g
    return x

isGarblable :: Program Circuit -> Bool
isGarblable = isJust . circ2tt'

--------------------------------------------------------------------------------
-- instances

instance Arbitrary Operation where
  arbitrary = elements [XOR, AND, OR, INPUT, NOT, CONST]

instance Arbitrary (Program Circuit) where
  arbitrary = arbCircuit `suchThat` isGarblable

arbCircuit :: Gen (Program Circuit)
arbCircuit = do
    (x,_) <- mkCircuit =<< vector 20
    let x' = do ref <- x; return [ref]
    return (L.buildCircuit x')

mkCircuit :: [Operation] -> Gen (L.Builder (Ref Circuit), [Operation])
mkCircuit (INPUT:ops) = do
    p <- elements [Garbler,Evaluator]
    return (L.input p, ops)

mkCircuit (CONST:ops) = do
    b <- arbitrary
    return (L.const b, ops)

mkCircuit (NOT:ops) = do
    (child, ops') <- mkCircuit ops
    return (L.not =<< child, ops')

mkCircuit (op:ops) = do
    (x,ops')  <- mkCircuit ops
    (y,ops'') <- mkCircuit ops'
    let c = bind2 (op2circ op) x y
    return (c, ops'')

mkCircuit [] = do
    p <- elements [Garbler,Evaluator]
    return (L.input p, [])

op2circ :: Operation -> Ref Circuit -> Ref Circuit -> L.Builder (Ref Circuit)
op2circ XOR x y = L.xor x y
op2circ AND x y = L.and x y
op2circ OR  x y = L.or  x y
op2circ _    _ _ = err "op2circ" "unsupported operation"
