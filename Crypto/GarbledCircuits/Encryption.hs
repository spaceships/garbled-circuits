{-# LANGUAGE PackageImports #-}

module Crypto.GarbledCircuits.Encryption
  (
    enc
  , dec
  , genKey
  , genR
  , randBlock
  , randBool
  , updateKey
  , updateR
  )
where

import Crypto.GarbledCircuits.Types
import Crypto.GarbledCircuits.Util

import qualified Data.ByteString    as BS
import           Control.Applicative
import           Control.Monad.State
import           Crypto.Cipher.AES
import           "crypto-random" Crypto.Random
import           Data.Bits ((.&.), (.|.))
import qualified Data.Bits          as Bits
import qualified Data.Serialize     as Ser
import           Data.Word

--------------------------------------------------------------------------------
-- encryption and decryption for wirelabels

-- AES-based garbling. Uses native hw instructions if available. Source:
-- https://web.engr.oregonstate.edu/~rosulekm/scbib/index.php?n=Paper.BHKR13
-- garbling: pi(K || T) xor K xor M where K = 2A xor 4B
--           where tweak = gateNum ++ colorX ++ colorY
--                 pi is publicly keyed block cipher (AES)
enc :: AES -> Ref GarbledGate -> Wirelabel -> Wirelabel -> Ciphertext -> Ciphertext
enc key gateRef x y z = encryptECB key (BS.append k tweak) `xor` k `xor` z
  where
    k     = double (wl_val x) `xor` double (double (wl_val y))
    tweak = Ser.encode (unRef gateRef, bit (wl_col x), bit (wl_col y))

    bit :: Bool -> Word32
    bit b = if b then 1 else 0

    double :: Ciphertext -> Ciphertext
    double c = BS.pack result
      where
        (xs, carry) = shiftLeft (BS.unpack c)
        result      = if carry > 0 then xorWords xs irreducible else xs

    irreducible :: [Word8]
    irreducible = replicate 15 0 ++ [86]

    shiftLeft :: [Word8] -> ([Word8], Word8)
    shiftLeft []     = ([], 0)
    shiftLeft (x:xs) = let (xs', c) = shiftLeft xs
                           msb = Bits.shiftR x 7
                           x'  = Bits.shiftL x 1 .|. c
                       in (x':xs', msb)

dec :: AES -> Ref GarbledGate -> Wirelabel -> Wirelabel -> Ciphertext -> Ciphertext
dec = enc

genKey :: Garble AES
genKey = initAES <$> randBlock

genR :: Garble Wirelabel
genR = Wirelabel True <$> randBlock

randBlock :: Garble Ciphertext
randBlock = do
  gen <- lift get
  let (blk, gen') = cprgGenerate 16 gen
  lift $ put gen'
  return blk

randBool :: Garble Bool
randBool = do
  gen <- lift get
  let (blk, gen') = cprgGenerate 1 gen
      w8          = head (BS.unpack blk)
  lift (put gen')
  return (w8 .&. 1 > 0)

updateKey :: AES -> Garble ()
updateKey k = lift.lift $ modify (\st -> st { ctx_key = k })

updateR :: Wirelabel -> Garble ()
updateR r = lift.lift $ modify (\st -> st { ctx_r = r })
