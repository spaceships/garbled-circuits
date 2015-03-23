module Test.ObliviousTransfer where

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
                         ]

prop_ddhCorrect :: Property
prop_ddhCorrect = once $ monadicIO $ do
    m <- run $ generate (choose (0,maxBound)) :: PropertyM IO Int
    res <- run $ runOT $ do 
      (pk, sk) <- ddhKeyGen 1024
      ct <- ddhEnc pk (fromIntegral m)
      ddhDec sk ct
    assert (res == fromIntegral m)
