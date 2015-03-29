module TestObliviousTransfer where

import            Data.Word
import Control.Concurrent
import Data.Functor
import Data.Maybe
import Data.Monoid
import Data.Serialize
import Crypto.Number.Serialize
import qualified Data.ByteString as BS

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Crypto.GarbledCircuits.ObliviousTransfer
import Crypto.GarbledCircuits.Network

obliviousTransferTests :: [Test]
obliviousTransferTests = [
                           testProperty "Diffie-Hellman is correct" prop_ddhCorrect
                         , testProperty "Messy mode OT is correct" prop_messyModeCorrect
                         , testProperty "Decryption mode OT is correct" prop_decModeCorrect
                         , testProperty "Can convert from Integer to ByteString" prop_convWorks
                         , testProperty "OT protocol works" prop_otProtoWorks
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

testMode :: OT CRS -> Property
testMode setup = once $ monadicIO $ do
    m <- pick $ choose (0,maxBound) :: PropertyM IO Int
    n <- pick $ choose (0,maxBound) :: PropertyM IO Int
    b <- pick arbitrary             :: PropertyM IO Bool
    (m',n') <- run $ runOT 1024 $ do
        crs <- setup
        (pk, sk) <- keyGen crs b
        (x,y)    <- enc crs pk (fromIntegral m, fromIntegral n)
        m' <- dec sk x
        n' <- dec sk y
        return (fromIntegral m', fromIntegral n')
    assert (sel b (m',n') == sel b (m,n))             -- can decrypt the chosen ciphertext
    assert (sel (not b) (m',n') /= sel (not b) (m,n)) -- and only the chosen ciphertext

prop_convWorks :: Property
prop_convWorks = monadicIO $ do
  bs <- BS.pack <$> pick (vectorOf 4 arbitrary)
  assert (bs == fromJust (i2ospOf 4 (os2ip bs)))

prop_otProtoWorks :: Property
prop_otProtoWorks = once $ monadicIO $ do
    m <- BS.pack <$> pick (vectorOf 16 arbitrary)
    n <- BS.pack <$> pick (vectorOf 16 arbitrary)
    b <- pick arbitrary
    port <- pick $ choose (1024,65536)
    run $ forkIO $ listenAt port (flip sendOne (m,n) . simpleConn)
    res <- run $ connectTo "localhost" port (flip recvOne b . simpleConn)
    assert (sel b (m,n) == res)

sel :: Bool -> (a,a) -> a
sel False (x,y) = x
sel True  (x,y) = y
