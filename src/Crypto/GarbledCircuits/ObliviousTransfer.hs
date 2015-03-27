{-# LANGUAGE PackageImports #-}

module Crypto.GarbledCircuits.ObliviousTransfer where

import Crypto.GarbledCircuits.Network
import Crypto.GarbledCircuits.Types
import Crypto.GarbledCircuits.Util (bind2)

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader

import Crypto.Number.Prime
import Crypto.Number.Generate
import Crypto.Number.ModArithmetic
{-import Crypto.Number.Serialize-}
import "crypto-random" Crypto.Random

-- diffie-hellman based dual mode oblivious transfer from https://eprint.iacr.org/2007/348
-- WARNING: this is a work in progress.

type SecretKey  = Integer
type Plaintext  = Integer
type Ciphertext = (Integer, Integer)

type CRS = (Integer, Integer, Integer, Integer)

type DHPublicKey = (Integer, Integer, Integer, Integer)
type OTPublicKey = (Integer, Integer)

type OT = ReaderT Integer (StateT SystemRNG IO)

otSend :: Connection -> [(ByteString, ByteString)] -> IO ()
otSend conn elements = undefined

otRecv :: Connection -> [Bool] -> IO [ByteString]
otRecv conn choices = undefined

recvOT :: Bool -> Connection -> IO Integer
recvOT sigma conn = do
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

sendOT :: (Integer, Integer) -> Connection -> IO ()
sendOT elems conn = do
    p <- recv conn
    gen <- cprgCreate <$> createEntropyPool
    flip evalStateT gen $ flip runReaderT p $ do
      crs <- recv4 conn
      pk  <- recv2 conn
      (c0,c1) <- enc crs pk elems
      send2 conn c0
      send2 conn c1

runOT :: Int -> OT a -> IO a
runOT n m = do
    gen <- cprgCreate <$> createEntropyPool
    let (p, gen') = generatePrime gen n
    evalStateT (runReaderT m p) gen'

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

enc :: CRS -> OTPublicKey -> (Integer, Integer) -> OT (Ciphertext, Ciphertext)
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
-- modular arithmetic in the OT monad

-- | Modular exponentiation using the prime p which is created in 'ddhKeyGen'
modExp :: Integer -> Integer -> OT Integer
modExp g x = expFast g x <$> ask

-- | Modular multiplication using the prime p which is created in 'ddhKeyGen'.
modMult :: Integer -> Integer -> OT Integer
modMult x y = mod (x * y) <$> ask

--------------------------------------------------------------------------------
-- random generator stuff

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

--------------------------------------------------------------------------------
-- helpers

send2 :: Connection -> (Integer, Integer) -> OT ()
send2 conn (x,y) = liftIO $ mapM_ (send conn) [x,y]

recv2 :: Connection -> OT (Integer, Integer)
recv2 conn = liftIO $ do [x,y] <- replicateM 2 (recv conn); return (x,y)

send4 :: Connection -> (Integer, Integer, Integer, Integer) -> OT ()
send4 conn (w,x,y,z) = liftIO $ mapM_ (send conn) [w,x,y,z]

recv4 :: Connection -> OT (Integer, Integer, Integer, Integer)
recv4 conn = liftIO $ do [w,x,y,z] <- replicateM 4 (recv conn); return (w,x,y,z)
