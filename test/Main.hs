{-# LANGUAGE PackageImports, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Concurrent
import Control.Monad
import "crypto-random" Crypto.Random
import Data.Functor
import Data.Maybe
import Data.Monoid
import Data.Serialize
import Data.Word
import qualified Data.Set as S
import Debug.Trace

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

main :: IO ()
main = defaultMainWithOpts tests mempty { ropt_color_mode = Just ColorAlways }

tests :: [Test]
tests = [ testProperty "The colors of new wirelabels are different" prop_colorsDifferent
        , testProperty "lsb R always equals 1" prop_lsbOfR
        , testProperty "Arbitrary circuit is correct" prop_arbitraryCircCorrect
        , testProperty "Reconstruct is correct" prop_reconstructCorrect
        , testProperty "Serialization is correct" prop_serializeCorrect
        , testProperty "Protocol works" prop_protoWorks
        ]

prop_colorsDifferent :: Property
prop_colorsDifferent = testGarble newWirelabels test
  where
    test p = lsb (wlp_true p) /= lsb (wlp_false p)

prop_lsbOfR :: Property
prop_lsbOfR = testGarble genR lsb

prop_arbitraryCircCorrect :: Program Circ -> Property
prop_arbitraryCircCorrect circ = monadicIO $ do
    (_,gg,ctx) <- testCirc circ
    inpA <- pick $ vector (inputSize PartyA circ)
    inpB <- pick $ vector (inputSize PartyB circ)
    let pt  = evalCirc  inpA inpB circ
        res = evalLocal inpA inpB (gg,ctx)
    assert (res == pt)

ensureEither :: Eq b => Either a b -> b -> Bool
ensureEither e eq = either (const False) (== eq) e

prop_reconstructCorrect :: Program Circ -> Property
prop_reconstructCorrect circ = monadicIO $ do
    (tt, gg, _) <- testCirc circ
    assert $ reconstruct tt (halfGates gg) == gg

prop_serializeCorrect :: Program Circ -> Property
prop_serializeCorrect circ = monadicIO $ do
    (tt, gg, _) <- testCirc circ
    assert $ ensureEither (reconstruct tt <$> decode (encode (halfGates gg))) gg

prop_protoWorks :: Program Circ -> Property
prop_protoWorks prog = once $ monadicIO $ do
    inpA <- pick $ vector (inputSize PartyA prog)
    inpB <- pick $ vector (inputSize PartyB prog)
    let pt = evalCirc inpA inpB prog
    port <- run $ generate (choose (1025,65536))
    chan <- run newChan
    run $ forkIO $ do
      res <- garblerProto port prog inpA
      writeChan chan res
    ggEval <- run $ evaluatorProto "localhost" port prog inpB
    ggGarb <- run $ readChan chan
    assert (ggEval == pt && ggGarb == pt)

--------------------------------------------------------------------------------
-- helpers

testCirc :: Program Circ -> PropertyM IO (Program TruthTable, Program GarbledGate, Context)
testCirc circ = do
    let tt = circ2tt circ
    (gg, ctx) <- run (tt2gg tt)
    return (tt, gg, ctx)

testGarble :: Garble a -> (a -> Bool) -> Property
testGarble g p = monadicIO $ do
    gen <- run $ fmap cprgCreate createEntropyPool
    let ((x, _), _) = runGarble' gen emptyProg $ do
          updateKey =<< genKey
          updateR   =<< genR
          g
    assert (p x)

isGarblable :: Program Circ -> Bool
isGarblable = isJust . circ2tt'

--------------------------------------------------------------------------------
-- instances

instance Arbitrary Operation where
  arbitrary = elements [XOR, AND, OR, INPUT, NOT, CONST]

instance Arbitrary (Program Circ) where
  arbitrary = arbCirc `suchThat` isGarblable

arbCirc :: Gen (Program Circ)
arbCirc = do
    (x,_) <- mkCircuit =<< vector 20
    let x' = do ref <- x; return [ref]
    return (buildCirc x')

mkCircuit :: [Operation] -> Gen (CircBuilder (Ref Circ), [Operation])
mkCircuit (INPUT:ops) = do
    p <- elements [PartyA,PartyB]
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
    p <- elements [PartyA,PartyB]
    return (L.input p, [])

op2circ :: Operation -> Ref Circ -> Ref Circ -> CircBuilder (Ref Circ)
op2circ XOR x y = L.xor x y
op2circ AND x y = L.and x y
op2circ OR  x y = L.or  x y
op2circ _    _ _ = err "op2circ" "unsupported operation"
