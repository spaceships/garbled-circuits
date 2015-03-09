{-# LANGUAGE PackageImports, FlexibleInstances #-}

module Main where

import Control.Monad
import "crypto-random" Crypto.Random
import Data.Functor
import Data.Monoid
import Data.Maybe
import Data.Word
import qualified Data.Set as S

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Monadic

import Crypto.GarbledCircuits
import Crypto.GarbledCircuits.Language
import Crypto.GarbledCircuits.GarbledGate
import Crypto.GarbledCircuits.TruthTable
import Crypto.GarbledCircuits.Encryption
import Crypto.GarbledCircuits.Evaluator
import Crypto.GarbledCircuits.Types
import Crypto.GarbledCircuits.Util

main :: IO ()
main = defaultMainWithOpts tests mempty { ropt_color_mode = Just ColorAlways }

tests = [ 
          testProperty "TruthTable 2 bit adder is correct"      prop_2BitAdderTT
        , testProperty "TruthTable 8 bit adder is correct"      prop_8BitAdderTT
        , testProperty "Garbled 2 bit adder is correct"         prop_2BitAdderGG
        , testProperty "Garbled 8 bit adder is correct"         prop_8BitAdderGG
        , testProperty "The colors of wirelabels are different" prop_colorsDifferent
        , testProperty "Arbitrary circuit is correct"           prop_arbitraryCirc
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
prop_colorsDifferent = testGarble new_wirelabels test
  where
    test p = wl_col (wlp_true p) /= wl_col (wlp_false p)

prop_arbitraryCirc :: Program Circ -> Property
prop_arbitraryCirc = testCirc

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

testCirc :: Program Circ -> Property
testCirc circ_test = monadicIO $ do
    let tt = circ2tt circ_test
    pre (isJust tt) -- ensure that the circ is garbleable
    garbled_test <- run (tt2gg tt)
    inp <- pick $ vector (inputSize circ_test)
    let pt  = evalCirc  inp circ_test
        gg  = evalLocal inp garbled_test
    assert (gg == pt)

--------------------------------------------------------------------------------
-- instances

instance Arbitrary Operation where
  arbitrary = elements [OXor, OAnd, OInput, ONot, OConst]

instance Arbitrary (Program Circ) where
  arbitrary = do
    (x,_) <- mkCircuit =<< vector 40
    let x' = do ref <- x; return [ref]
    return (buildCirc x')
     
mkCircuit :: [Operation] -> Gen (CircBuilder (Ref Circ), [Operation])
mkCircuit (OInput:ops) = do
    return (c_input, ops)

mkCircuit (OConst:ops) = do
    b <- arbitrary
    return (c_const b, ops)

mkCircuit (ONot:ops) = do
    (child, ops') <- mkCircuit ops
    return (c_not =<< child, ops')

mkCircuit (op:ops) = do
    (x,ops')  <- mkCircuit ops
    (y,ops'') <- mkCircuit ops'
    let c = bindM2 (op2circ op) x y
    return (c, ops'')

mkCircuit [] = return (c_input, [])

op2circ :: Operation -> Ref Circ -> Ref Circ -> CircBuilder (Ref Circ)
op2circ OXor x y = c_xor x y
op2circ OAnd x y = c_and x y
op2circ _    _ _ = err "op2circ" "unsupported operation"

inputSize :: Program Circ -> Int
inputSize = S.size . prog_inputs
