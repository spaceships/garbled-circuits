{-# LANGUAGE PackageImports #-}

module Crypto.GarbledCircuits.Encryption
  ( enc
  , dec
  , genKey
  , genR
  , hash
  , randBlock
  , randBool
  , updateKey
  , pad
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

-- The AES-based hash function from the halfgates paper (p8)
hash :: AES -> Wirelabel -> Int -> Wirelabel
hash key x i = encryptECB key k `xor` k
  where
    k = double x `xor` pad 16 (Ser.encode i)

-- AES-based garbling. Uses native hw instructions if available. Source:
-- https://web.engr.oregonstate.edu/~rosulekm/scbib/index.php?n=Paper.BHKR13
-- garbling: pi(K) xor K xor M where K = 2A xor 4B xor T
--           where tweak = gateNum ++ colorX ++ colorY
--                 pi is publicly keyed block cipher (AES)
enc :: AES -> Ref GarbledGate -> Wirelabel -> Wirelabel -> Wirelabel -> Wirelabel
enc key gateRef a b m = encryptECB key k `xor` k `xor` m
  where
    t = Ser.encode (unRef gateRef, bit (lsb a), bit (lsb b))
    k = double a `xor` double (double b) `xor` t

dec :: AES -> Ref GarbledGate -> Wirelabel -> Wirelabel -> Wirelabel -> Wirelabel
dec = enc

pad :: Int -> ByteString -> ByteString
pad n ct = BS.append (BS.replicate (n - BS.length ct) 0) ct

bit :: Bool -> Word32
bit b = if b then 1 else 0

double :: ByteString -> ByteString
double c = BS.pack result
  where
    (xs, carry) = shiftLeft (BS.unpack c)
    result      = if carry > 0 then xorWords xs irreducible else xs

irreducible :: [Word8]
irreducible = replicate 15 0 ++ [86]

shiftLeft :: [Word8] -> ([Word8], Word8)
shiftLeft []     = ([], 0)
shiftLeft (b:bs) = let (bs', c) = shiftLeft bs
                       msb = Bits.shiftR b 7
                       b'  = Bits.shiftL b 1 .|. c
                   in (b':bs', msb)

genKey :: Garble AES
genKey = initAES <$> randBlock

genR :: Garble Wirelabel
genR = do
    b <- randBlock
    let color = pad 16 $ Ser.encode (1 :: Int)
        wl    = bitOr color b
    return wl

randBlock :: Garble ByteString
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
