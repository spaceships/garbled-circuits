module Crypto.GarbledCircuits.Encryption
  ( genKey
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
import           Control.Monad.State
import           Crypto.Cipher.AES128
import           Data.Bits ((.&.), (.|.))
import qualified Data.Bits          as Bits
import           Data.Maybe
import qualified Data.Serialize     as Ser
import           Data.Word
import           Data.Functor
import           System.Entropy

--------------------------------------------------------------------------------
-- encryption and decryption for wirelabels

-- The AES-based hash function from the halfgates paper (p8)
-- Uses native hw instructions if available
hash :: AESKey128 -> ByteString -> Int -> ByteString
hash key x i = encryptBlock key k `xorBytes` k
  where
    k = double x `xorBytes` pad 16 (Ser.encode i)

pad :: Int -> ByteString -> ByteString
pad n ct = BS.append (BS.replicate (n - BS.length ct) 0) ct

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

genKey :: Garble AESKey128
genKey = do
  key <- randBlock
  let k = fromMaybe (err "genKey" "bad key") (buildKey key)
  return k

genR :: Garble Wirelabel
genR = do
    b <- randBlock
    let color = pad 16 $ Ser.encode (1 :: Int)
        wl    = orBytes color b
    return wl

randBlock :: Garble ByteString
randBlock = liftIO (getEntropy 16)

randBool :: Garble Bool
randBool = do
  w8 <- BS.head <$> liftIO (getEntropy 1)
  return (w8 .&. 1 > 0)

updateKey :: AESKey128 -> Garble ()
updateKey k = lift.lift $ modify (\st -> st { ctx_key = k })

updateR :: Wirelabel -> Garble ()
updateR r = lift.lift $ modify (\st -> st { ctx_r = r })
