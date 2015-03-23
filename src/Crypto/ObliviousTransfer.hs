{-# LANGUAGE PackageImports #-}

module Crypto.ObliviousTransfer where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader

import Crypto.Cipher.AES128 (AESKey128)
import Crypto.Number.Prime
import Crypto.Number.Generate
import Crypto.Number.ModArithmetic
import "crypto-random" Crypto.Random

{-import qualified Data.ByteString as BS-}
{-import Data.Bits-}

-- diffie-hellman based dual mode oblivious transfer from https://eprint.iacr.org/2007/348

type SecretKey  = Integer
type Plaintext  = Integer
type Ciphertext = (Integer, Integer)

type CRS = (Integer, Integer, Integer, Integer)

type DHPublicKey = (Integer, Integer, Integer, Integer)
type OTPublicKey = (Integer, Integer)

type MessyTrapdoor = (Integer, Integer)
type DecTrapdoor   = Integer

type OT = ReaderT Integer (State SystemRNG)

runOT :: Int -> OT a -> IO a
runOT n m = do
    gen <- cprgCreate <$> createEntropyPool
    let (p, gen') = generatePrime gen n
    return $ evalState (runReaderT m p) gen'

setupMessy :: OT (CRS, MessyTrapdoor)
setupMessy = do
    g0 <- randGenerator
    g1 <- randGenerator
    (x0,x1) <- randZpDistinct
    h0 <- modExp g0 x0
    h1 <- modExp g1 x1
    return ((g0,h0,g1,h1), (x0,x1))

setupDec :: OT (CRS, DecTrapdoor)
setupDec = do
    g0 <- randGenerator
    y  <- randZp
    g1 <- modExp g0 y
    x  <- randZp
    h0 <- modExp g0 x
    h1 <- modExp g1 x
    return ((g0,h0,g1,h1),y)

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

findMessy :: MessyTrapdoor -> OTPublicKey -> OT Bool
findMessy (x0,x1) (g,h) = do
    h' <- modExp g x0
    return (h /= h')

trapKeyGen :: DecTrapdoor -> OT (OTPublicKey, Integer, Integer)
trapKeyGen = undefined

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
    p <- ask
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

{-randBool :: OT Bool-}
{-randBool = do-}
    {-bs <- rand (\g -> cprgGenerate 1 g)-}
    {-return (BS.head bs .&. 1 > 0)-}

-- | Do the RNG bookkeeping on a function taking and producing one.
rand :: (SystemRNG -> (a, SystemRNG)) -> OT a
rand f = do
    g <- get
    let (a, g') = f g
    put g'
    return a

--------------------------------------------------------------------------------
-- helpers

bind2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bind2 f a b = do x <- a; y <- b; f x y
