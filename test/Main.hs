{-# LANGUAGE FlexibleInstances #-}

module Main where

import Data.Functor
import Data.Monoid
import Crypto.Random

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Monadic

import Crypto.GarbledCircuits
import Crypto.GarbledCircuits.Language
import Crypto.GarbledCircuits.GarbledGate
import Crypto.GarbledCircuits.Encryption
import Crypto.GarbledCircuits.Evaluator
import Crypto.GarbledCircuits.Types
import Crypto.GarbledCircuits.Util

main :: IO ()
main = defaultMainWithOpts tests mempty { ropt_color_mode = Just ColorAlways }

tests = [ testProperty "TruthTable 2 bit adder is correct"            prop_2BitAdderTT
        , testProperty "Garbled 2 bit adder is correct"               prop_2BitAdderGG
        , testProperty "The colors of input wirelabels are different" prop_inputColorsDifferent
        , testProperty "All boolean garbled gates area correct"       prop_booleanGatesCorrect
        , testProperty "We correctly evaluate a mixed circuit"        prop_mixedCirc
        ]

prop_2BitAdderTT :: (Bool, Bool) -> (Bool, Bool) -> Bool
prop_2BitAdderTT x y = eval_2BitAdder x y == eval_2BitAdderTT x y

prop_2BitAdderGG :: (Bool, Bool) -> (Bool, Bool) -> Property
prop_2BitAdderGG x y = monadicIO $ do
  let pt = eval_2BitAdder x y
  gg <- run $ eval_2BitAdderGG x y
  assert (gg == pt)

prop_inputColorsDifferent :: Property
prop_inputColorsDifferent = testGarble new_wirelabels test
  where
    test p = wl_col (wlp_true p) /= wl_col (wlp_false p)

prop_booleanGatesCorrect :: Operation -> Bool -> Bool -> Property
prop_booleanGatesCorrect op = testCirc circ_test
  where
    circ_test = buildCirc $ do
      x <- c_input
      y <- c_input
      z <- intern (op2circ op [x,y])
      return [z]

prop_mixedCirc :: Bool -> Bool -> Bool -> Bool -> Property
prop_mixedCirc = testCirc4 circ_test
  where
    circ_test = buildCirc $ do
      r0 <- c_input
      r1 <- c_input
      r2 <- c_input
      r3 <- c_input
      r4 <- c_and r0 r2
      r6 <- c_xor r1 r3
      r7 <- c_xor r4 r6
      return [r7]

--------------------------------------------------------------------------------
-- helpers

testGarble :: Garble a -> (a -> Bool) -> Property
testGarble g p = monadicIO $ do
    gen <- run $ fmap cprgCreate createEntropyPool
    let ((x, prog), ctx) = runGarble' gen emptyProg $ do
          updateKey =<< genKey
          updateR   =<< genR
          g
    assert (p x) 

instance Arbitrary Operation where
  arbitrary = elements [OXor, OAnd, OOr]

testCirc :: Program Circ -> Bool -> Bool -> Property
testCirc circ_test x y = monadicIO $ do
    garbled_test <- run (garble circ_test)
    let pt = evalCirc  [x,y] circ_test
        gg = evalLocal [x,y] garbled_test
    assert (gg == pt)

testCirc3 :: Program Circ -> Bool -> Bool -> Bool -> Property
testCirc3 circ_test x y z = monadicIO $ do
    garbled_test <- run (garble circ_test)
    let pt = evalCirc  [x,y,z] circ_test
        gg = evalLocal [x,y,z] garbled_test
    assert (gg == pt)

testCirc4 :: Program Circ -> Bool -> Bool -> Bool -> Bool -> Property
testCirc4 circ_test x y z w = monadicIO $ do
    garbled_test <- run (garble circ_test)
    let pt = evalCirc  [x,y,z,w] circ_test
        gg = evalLocal [x,y,z,w] garbled_test
    assert (gg == pt)

