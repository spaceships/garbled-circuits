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
    m <- run $ generate (choose (0,maxBound)) :: PropertyM IO Int
    res <- run $ runOT 1024 $ do 
      (pk, sk) <- ddhKeyGen
      ct <- ddhEnc pk (fromIntegral m)
      ddhDec sk ct
    assert (res == fromIntegral m)

prop_messyModeCorrect :: Property
prop_messyModeCorrect = monadicIO $ do
    m <- run $ generate (choose (0,maxBound)) :: PropertyM IO Int
    n <- run $ generate (choose (0,maxBound)) :: PropertyM IO Int
    b <- run $ generate arbitrary             :: PropertyM IO Bool
    res <- run $ runOT 1024 $ do
        (crs, t) <- setupMessy
        (pk, sk) <- keyGen crs b
        (x,y)    <- enc crs pk (fromIntegral m, fromIntegral n)
        if b then dec sk y else dec sk x
    assert (fromIntegral res == sel b (m,n))

prop_decModeCorrect :: Property
prop_decModeCorrect = monadicIO $ do
    m <- run $ generate (choose (0,maxBound)) :: PropertyM IO Int
    n <- run $ generate (choose (0,maxBound)) :: PropertyM IO Int
    b <- run $ generate arbitrary             :: PropertyM IO Bool
    res <- run $ runOT 1024 $ do
        (crs, t) <- setupDec
        (pk, sk) <- keyGen crs b
        (x,y)    <- enc crs pk (fromIntegral m, fromIntegral n)
        if b then dec sk y else dec sk x
    assert (fromIntegral res == sel b (m,n))

sel :: Bool -> (a,a) -> a
sel False (x,y) = x
sel True  (x,y) = y
