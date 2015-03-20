{-# LANGUAGE PackageImports, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Concurrent
import Control.Monad
import "crypto-random" Crypto.Random
import Data.Functor
import Data.Monoid
import Data.Maybe
import Data.Word
import Data.Serialize
import qualified Data.Set as S

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Crypto.GarbledCircuits
import Crypto.GarbledCircuits.Language
import Crypto.GarbledCircuits.GarbledGate
import Crypto.GarbledCircuits.TruthTable
import Crypto.GarbledCircuits.Encryption
import Crypto.GarbledCircuits.Eval
import Crypto.GarbledCircuits.Types
import Crypto.GarbledCircuits.Util

import Example.Adder

main :: IO ()
main = defaultMainWithOpts tests mempty { ropt_color_mode = Just ColorAlways }

tests :: [Test]
tests = [
          testProperty "TruthTable 2 bit adder is correct" prop_2BitAdderTT
        , testProperty "TruthTable 8 bit adder is correct" prop_8BitAdderTT
        , testProperty "Garbled 2 bit adder is correct" prop_2BitAdderGG
        , testProperty "Garbled 8 bit adder is correct" prop_8BitAdderGG
        , testProperty "The colors of new wirelabels are different" prop_colorsDifferent
        , testProperty "lsb R always equals 1" prop_lsbOfR
        , testProperty "Arbitrary circuit is correct" prop_arbitraryCircCorrect
        , testProperty "Reconstruct is correct" prop_reconstructCorrect
        , testProperty "Serialization is correct" prop_serializeCorrect
        {-, testProperty "The networking protocols work" prop_protoWorks-}
        ]

prop_2BitAdderTT :: (Bool, Bool) -> (Bool, Bool) -> Bool
prop_2BitAdderTT x y = eval_2BitAdder x y == eval_2BitAdderTT x y

prop_8BitAdderTT :: Word8 -> Word8 -> Bool
prop_8BitAdderTT x y = eval_8BitAdder x y == eval_8BitAdderTT x y

prop_2BitAdderGG :: (Bool, Bool) -> (Bool, Bool) -> Property
prop_2BitAdderGG x y = monadicIO $ do
  let pt = eval_2BitAdder x y
  gg <- run $ eval_2BitAdderGG x y
  assert (gg == pt)

prop_8BitAdderGG :: Word8 -> Word8 -> Property
prop_8BitAdderGG x y = monadicIO $ do
  let pt = eval_8BitAdder x y
  gg <- run $ eval_8BitAdderGG x y
  assert (gg == pt)

prop_colorsDifferent :: Property
prop_colorsDifferent = testGarble newWirelabels test
  where
    test p = lsb (wlp_true p) /= lsb (wlp_false p)

prop_lsbOfR :: Property
prop_lsbOfR = testGarble genR lsb

prop_arbitraryCircCorrect :: Program Circ -> Property
prop_arbitraryCircCorrect circ = monadicIO $ do
    (_,gg,ctx) <- testCirc circ
    inpA <- pick $ vector (inputSize A circ)
    inpB <- pick $ vector (inputSize B circ)
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
prop_protoWorks prog = monadicIO $ do
    inpA <- pick $ vector (inputSize A prog)
    inpB <- pick $ vector (inputSize B prog)
    let pt = evalCirc inpA inpB prog
    run $ forkIO $ void (garblerProto 12345 prog inpA)
    gg <- run $ evaluatorProto "localhost" 12345 prog inpB
    assert (gg == pt)

--------------------------------------------------------------------------------
-- helpers

testCirc :: Program Circ -> PropertyM IO (Program TruthTable, Program GarbledGate, Context)
testCirc circ = do
    let tt = circ2tt' circ
    pre (isJust tt) -- ensure that the circ is garbleable
    (gg, ctx) <- run (tt2gg (fromJust tt))
    return (fromJust tt, gg, ctx)

testGarble :: Garble a -> (a -> Bool) -> Property
testGarble g p = monadicIO $ do
    gen <- run $ fmap cprgCreate createEntropyPool
    let ((x, _), _) = runGarble' gen emptyProg $ do
          updateKey =<< genKey
          updateR   =<< genR
          g
    assert (p x)

--------------------------------------------------------------------------------
-- instances

instance Arbitrary Operation where
  arbitrary = elements [XOR, AND, OR, INPUT, NOT, CONST]

instance Arbitrary (Program Circ) where
  arbitrary = do
    (x,_) <- mkCircuit =<< vector 20
    let x' = do ref <- x; return [ref]
    return (buildCirc x')

mkCircuit :: [Operation] -> Gen (CircBuilder (Ref Circ), [Operation])
mkCircuit (INPUT:ops) = do
    p <- elements [A,B]
    return (c_input p, ops)

mkCircuit (CONST:ops) = do
    b <- arbitrary
    return (c_const b, ops)

mkCircuit (NOT:ops) = do
    (child, ops') <- mkCircuit ops
    return (c_not =<< child, ops')

mkCircuit (op:ops) = do
    (x,ops')  <- mkCircuit ops
    (y,ops'') <- mkCircuit ops'
    let c = bind2 (op2circ op) x y
    return (c, ops'')

mkCircuit [] = do
    p <- elements [A,B]
    return (c_input p, [])

op2circ :: Operation -> Ref Circ -> Ref Circ -> CircBuilder (Ref Circ)
op2circ XOR x y = c_xor x y
op2circ AND x y = c_and x y
op2circ OR  x y = c_or  x y
op2circ _    _ _ = err "op2circ" "unsupported operation"
