{-# LANGUAGE PackageImports #-}

module Crypto.GarbledCircuits.ObliviousTransfer where

import Crypto.GarbledCircuits.Network
import Crypto.GarbledCircuits.Types
import Crypto.GarbledCircuits.Util
import Crypto.GarbledCircuits.Encryption hiding (randBool)

import Crypto.Cipher.AES128 (AESKey128)
import Crypto.Number.Prime
import Crypto.Number.Generate
import Crypto.Number.ModArithmetic
import Crypto.Number.Serialize
import "crypto-random" Crypto.Random

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Data.Maybe
import Data.Bits
import Data.Word
import Data.Serialize hiding (get, put)
import Data.List.Split (chunksOf)
import qualified Data.ByteString as BS

-- diffie-hellman based dual mode oblivious transfer from https://eprint.iacr.org/2007/348
-- WARNING: this is a work in progress.

keySize = 2048

type SecretKey  = Integer
type Plaintext  = Integer
type Ciphertext = (Integer, Integer)
type CRS = (Integer, Integer, Integer, Integer)
type OTPublicKey = (Integer, Integer)

type OT = ReaderT Integer (StateT SystemRNG IO)

runOT :: Int -> OT a -> IO a
runOT n m = do
    gen <- cprgCreate <$> createEntropyPool
    let (p, gen') = generatePrime gen n
    evalStateT (runReaderT m p) gen'

--------------------------------------------------------------------------------
-- ot extension
-- https://web.engr.oregonstate.edu/~rosulekm/scbib/index.php?n=Paper.IKNP03

k = 1 -- number of OTs

otSend :: Connection -> AESKey128 -> [(ByteString, ByteString)] -> IO ()
otSend conn key elems = do
    let n = length elems
    g0 <- newGen
    let (s, g1) = randBits k g0
    q <- forM s $ \b -> do
      j <- recvOT conn b
      return j
    let rows = fmap (pad 16 . bits2Bytes) (tr $ fmap (bytes2Bits n) q)
    forM_ (zip3 [0..] rows elems) $ \(i, row, (x,y)) -> do
      let ctx = x `xorBytes` hash key row i
      let cty = y `xorBytes` hash key (row `xorBytes` pad 16 (bits2Bytes s)) i
      send2 conn (ctx, cty)

otRecv :: Connection -> AESKey128 -> [Bool] -> IO [ByteString]
otRecv conn key choices = do
    let n = length choices
        r = bits2Bytes choices
    g0 <- newGen
    let (t, g1) = randBitMatrix (n, k) g0
    forM (tr t) $ \col -> do
      let j = bits2Bytes col
      sendOT conn (pad 16 j, pad 16 j `xorBytes` pad 16 r)
    forM (zip3 [0..] choices t) $ \(i, b, row) -> do
      (x,y) <- recv2 conn
      let c = if b then y else x
      let m = pad 16 c `xorBytes` hash key (pad 16 $ bits2Bytes row) i
      return m

randBytes :: Int -> SystemRNG -> (ByteString, SystemRNG)
randBytes = cprgGenerate

randBits :: Int -> SystemRNG -> ([Bool], SystemRNG)
randBits n g = (fmap fst bits, snd (last bits))
  where
    bits = take n $ tail $ iterate (\(_, g) -> randBool g) (False, g)

randBitMatrix :: (Int, Int) -> SystemRNG -> ([[Bool]], SystemRNG)
randBitMatrix (height, width) g0 = (chunksOf width bits, g1)
  where
    (bits, g1) = randBits (height * width) g0

randBool :: SystemRNG -> (Bool, SystemRNG)
randBool g0 = ((byte .&. 1) > 0, g1)
  where
    (b, g1) = cprgGenerate 1 g0
    byte    = BS.head b

--------------------------------------------------------------------------------
-- dual mode ot
-- TODO find a more implementable OT

-- only intended to share 128 bit secrets
sendOT :: Connection -> (ByteString, ByteString) -> IO ()
sendOT conn (x,y) = sendDual conn (os2ip x, os2ip y)

-- only intended to share 128 bit secrets
recvOT :: Connection -> Bool -> IO ByteString
recvOT conn sigma = fromJust <$> i2ospOf 16 <$> recvDual conn sigma

recvDual :: Connection -> Bool -> IO Plaintext
recvDual conn sigma = do
    gen <- newGen
    let (p, gen') = generatePrime gen keySize
    traceM ("p=" ++ show p)
    send conn p
    flip evalStateT gen' $ flip runReaderT p $ do
      crs      <- setupMessy
      (pk, sk) <- keyGen crs sigma
      liftIO $ send4 conn crs
      liftIO $ send2 conn pk
      c0 <- liftIO $ recv2 conn
      c1 <- liftIO $ recv2 conn
      if sigma then dec sk c1 else dec sk c0

sendDual :: Connection -> (Plaintext, Plaintext) -> IO ()
sendDual conn elems = do
    p <- recv conn
    when (fst elems > p) $ err "sendDual" "arg0 greater than p"
    when (snd elems > p) $ err "sendDual" "arg1 greater than p"
    gen <- newGen
    flip evalStateT gen $ flip runReaderT p $ do
      crs <- liftIO $ recv4 conn
      pk  <- liftIO $ recv2 conn
      (c0,c1) <- enc crs pk elems
      liftIO $ send2 conn c0
      liftIO $ send2 conn c1

setupMessy :: OT CRS
setupMessy = do
    g0 <- randGenerator
    g1 <- randGenerator
    (x0,x1) <- randZpDistinct
    h0 <- modExp g0 x0
    h1 <- modExp g1 x1
    return (g0,h0,g1,h1)

setupDec :: OT CRS
setupDec = do
    g0 <- randGenerator
    y  <- randZp
    g1 <- modExp g0 y
    x  <- randZp
    h0 <- modExp g0 x
    h1 <- modExp g1 x
    return (g0,h0,g1,h1)

keyGen :: CRS -> Bool -> OT (OTPublicKey, SecretKey)
keyGen (g0,h0,g1,h1) b = do
    let (g',h') = if b then (g1,h1) else (g0,h0)
    r <- randZp
    g <- modExp g' r
    h <- modExp h' r
    return ((g,h), r)

enc :: CRS -> OTPublicKey -> (Plaintext, Plaintext) -> OT (Ciphertext, Ciphertext)
enc (g0,h0,g1,h1) (g,h) (m0,m1) = do
    let pk0 = (g0, h0, g, h)
        pk1 = (g1, h1, g, h)
    c0 <- ddhEnc pk0 m0
    c1 <- ddhEnc pk1 m1
    return (c0,c1)

dec :: SecretKey -> Ciphertext -> OT Plaintext
dec = ddhDec

--------------------------------------------------------------------------------
-- modified diffie hellman

type DHPublicKey = (Integer, Integer, Integer, Integer)

ddhKeyGen :: OT (DHPublicKey, SecretKey)
ddhKeyGen = do
    g  <- randGenerator
    h  <- randGenerator
    x  <- randZp
    g' <- modExp g x
    h' <- modExp h x
    let pk = (g, h, g', h')
    return (pk, x)

ddhEnc :: DHPublicKey -> Plaintext -> OT Ciphertext
ddhEnc pk m = do
    (u,v) <- randomize pk
    res   <- modMult v m
    return (u, res)

randomize :: DHPublicKey -> OT (Integer, Integer)
randomize (g,h,g',h') = do
    s <- randZp
    t <- randZp
    u <- bind2 modMult (modExp g  s) (modExp h  t)
    v <- bind2 modMult (modExp g' s) (modExp h' t)
    return (u,v)

ddhDec :: SecretKey -> Ciphertext -> OT Plaintext
ddhDec sk (c0, c1) = do
    p <- ask
    modMult c1 =<< (inverseCoprimes <$> modExp c0 sk <*> pure p)

--------------------------------------------------------------------------------
-- helpers

tr :: [[a]] -> [[a]]
tr xs | null xs        = []
      | null (head xs) = []
      | otherwise      = fmap head xs : tr (fmap tail xs)

newGen :: IO SystemRNG
newGen = cprgCreate <$> createEntropyPool

bits2Bytes :: [Bool] -> ByteString
bits2Bytes = BS.pack . fmap bits2Word . chunksOf 8

bytes2Bits :: Int -> ByteString -> [Bool]
bytes2Bits n = take n . concatMap word2Bits . BS.unpack

--------------------------------------------------------------------------------
-- OT monad helpers

-- | Modular exponentiation using the prime p which is created in 'ddhKeyGen'
modExp :: Integer -> Integer -> OT Integer
modExp g x = expFast g x <$> ask

-- | Modular multiplication using the prime p which is created in 'ddhKeyGen'.
modMult :: Integer -> Integer -> OT Integer
modMult x y = mod (x * y) <$> ask

randGenerator :: OT Integer
randGenerator = randRange 2

randZp :: OT Integer
randZp = randRange 1

randZpDistinct :: OT (Integer, Integer)
randZpDistinct = do
    x <- randZp
    y <- randZp
    if x == y then
      randZpDistinct
    else
      return (x,y)

-- | Get a random integer in the range (low, p-1) in the 'OT' monad
randRange :: Integer -> OT Integer
randRange low = do
    p <- ask
    rand (\gen -> generateBetween gen low p)

-- | Do the RNG bookkeeping on a function taking and producing one.
rand :: (SystemRNG -> (a, SystemRNG)) -> OT a
rand f = do
    g <- get
    let (a, g') = f g
    put g'
    return a
