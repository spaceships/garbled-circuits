module TestObliviousTransfer where

import Control.Concurrent
import Data.Functor
import Data.Maybe
import Data.Monoid
import Data.Serialize

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Crypto.ObliviousTransfer

obliviousTransferTests :: [Test]
obliviousTransferTests = [
                           testProperty "Diffie-Hellman is correct" prop_ddhCorrect
                         , testProperty "Messy mode OT is correct" prop_messyModeCorrect
                         , testProperty "Decryption mode OT is correct" prop_decModeCorrect
                         ]

prop_ddhCorrect :: Property
prop_ddhCorrect = once $ monadicIO $ do
    m <- pick $ choose (0,maxBound) :: PropertyM IO Int
    res <- run $ runOT 1024 $ do 
      (pk, sk) <- ddhKeyGen
      ct <- ddhEnc pk (fromIntegral m)
      ddhDec sk ct
    assert (res == fromIntegral m)

prop_messyModeCorrect :: Property
prop_messyModeCorrect = testMode setupMessy

prop_decModeCorrect :: Property
prop_decModeCorrect = testMode setupDec

testMode :: (OT (CRS, t)) -> Property
testMode setup = once $ monadicIO $ do
    m <- pick $ choose (0,maxBound) :: PropertyM IO Int
    n <- pick $ choose (0,maxBound) :: PropertyM IO Int
    b <- pick arbitrary             :: PropertyM IO Bool
    (m',n') <- run $ runOT 1024 $ do
        (crs, t) <- setup
        (pk, sk) <- keyGen crs b
        (x,y)    <- enc crs pk (fromIntegral m, fromIntegral n)
        m' <- dec sk x
        n' <- dec sk y
        return (fromIntegral m', fromIntegral n')
    assert (sel b (m',n') == sel b (m,n))             -- can decrypt the chosen ciphertext
    assert (sel (not b) (m',n') /= sel (not b) (m,n)) -- and only the chosen ciphertext

sel :: Bool -> (a,a) -> a
sel False (x,y) = x
sel True  (x,y) = y
