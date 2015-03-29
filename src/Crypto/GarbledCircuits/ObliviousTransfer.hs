{-# LANGUAGE PackageImports #-}

module Crypto.GarbledCircuits.ObliviousTransfer where

import Crypto.GarbledCircuits.Network
import Crypto.GarbledCircuits.Types
import Crypto.GarbledCircuits.Util (err, bind2, traceM)

import Crypto.Number.Prime
import Crypto.Number.Generate
import Crypto.Number.ModArithmetic
import Crypto.Number.Serialize
import "crypto-random" Crypto.Random

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Data.Maybe

-- diffie-hellman based dual mode oblivious transfer from https://eprint.iacr.org/2007/348
-- WARNING: this is a work in progress.

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

otSend :: Connection -> [(ByteString, ByteString)] -> IO ()
otSend conn elements = undefined

otRecv :: Connection -> [Bool] -> IO [ByteString]
otRecv conn choices = undefined

--------------------------------------------------------------------------------
-- dual mode ot)

-- only intended to share 128 bit secrets
sendOne :: Connection -> (ByteString, ByteString) -> IO ()
sendOne conn (x,y) = sendDual conn (os2ip x, os2ip y)

-- only intended to share 128 bit secrets
recvOne :: Connection -> Bool -> IO ByteString
recvOne conn sigma = fromJust <$> i2ospOf 16 <$> recvDual conn sigma

recvDual :: Connection -> Bool -> IO Plaintext
recvDual conn sigma = do
    gen <- cprgCreate <$> createEntropyPool
    let (p, gen') = generatePrime gen 1024
    send conn p
    flip evalStateT gen' $ flip runReaderT p $ do
      crs      <- setupMessy
      (pk, sk) <- keyGen crs sigma
      send4 conn crs
      send2 conn pk
      c0 <- recv2 conn
      c1 <- recv2 conn
      if sigma then dec sk c1 else dec sk c0

sendDual :: Connection -> (Plaintext, Plaintext) -> IO ()
sendDual conn elems = do
    p <- recv conn
    when (fst elems > p) $ err "sendOT" "arg0 greater than p"
    when (snd elems > p) $ err "sendOT" "arg1 greater than p"
    gen <- cprgCreate <$> createEntropyPool
    flip evalStateT gen $ flip runReaderT p $ do
      crs <- recv4 conn
      pk  <- recv2 conn
      (c0,c1) <- enc crs pk elems
      send2 conn c0
      send2 conn c1

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

send2 :: Connection -> (Integer, Integer) -> OT ()
send2 conn (x,y) = liftIO $ do
    n <- sum <$> mapM (send' conn) [x,y]
    traceM ("[send2] sent " ++ show n ++ " bytes")

recv2 :: Connection -> OT (Integer, Integer)
recv2 conn = liftIO $ do
    res <- replicateM 2 (recv' conn)
    let [x,y] = map fst res
        n = sum (map snd res)
    traceM ("[recv2] recieved " ++ show n ++ " bytes")
    return (x,y)

send4 :: Connection -> (Integer, Integer, Integer, Integer) -> OT ()
send4 conn (w,x,y,z) = liftIO $ do
    n <- sum <$> mapM (send' conn) [w,x,y,z]
    traceM ("[send4] sent " ++ show n ++ " bytes")

recv4 :: Connection -> OT (Integer, Integer, Integer, Integer)
recv4 conn = liftIO $ do
    res <- replicateM 4 (recv' conn)
    let [w,x,y,z] = map fst res
        n = sum (map snd res)
    traceM ("[recv4] recieved " ++ show n ++ " bytes")
    return (w,x,y,z)
