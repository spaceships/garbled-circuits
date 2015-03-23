{-# LANGUAGE PackageImports #-}

module Crypto.ObliviousTransfer where

import           Control.Applicative
import           Control.Monad.State
import           Crypto.Cipher.AES128 (AESKey128)
import           Crypto.Number.Prime
import           Crypto.Number.Generate
import           Crypto.Number.ModArithmetic
import           "crypto-random" Crypto.Random

-- diffie-hellman based dual mode oblivious transfer from https://eprint.iacr.org/2007/348

data OTSt = OTSt { ot_rng :: SystemRNG, ot_mod :: Integer }
type OT = State OTSt

type PublicKey  = (Integer, Integer, Integer, Integer)
type SecretKey  = Integer
type Plaintext  = Integer
type Ciphertext = (Integer, Integer)

runOT :: OT a -> IO a
runOT m = do
    rng <- cprgCreate <$> createEntropyPool
    let st = OTSt rng (error "[runOT] undefined modulus")
    return $ evalState m st

modExp :: Integer -> Integer -> OT Integer
modExp g x = expFast g x <$> gets ot_mod

modMult :: Integer -> Integer -> OT Integer
modMult x y = mod (x * y) <$> gets ot_mod

randRange :: (Integer, Integer) -> OT Integer
randRange (low, high) = rand (\gen -> generateBetween gen low high)

randPrime :: Int -> OT Integer
randPrime n = rand (\gen -> generatePrime gen n)

rand :: (SystemRNG -> (a, SystemRNG)) -> OT a
rand f = do
    g <- gets ot_rng
    let (a, g') = f g
    modify $ \st -> st { ot_rng = g' }
    return a

ddhKeyGen :: Int -> OT (PublicKey, SecretKey)
ddhKeyGen n = do
    p <- randPrime n
    modify $ \st -> st { ot_mod = p }
    g <- randRange (2,p-1)
    h <- randRange (2,p-1)
    x <- randRange (1,p-1)
    g' <- modExp g x
    h' <- modExp h x
    let pk = (g, h, g', h')
    return (pk, x)

ddhEnc :: PublicKey -> Plaintext -> OT Ciphertext
ddhEnc pk m = do
    (u,v) <- randomize pk
    res   <- modMult v m
    return (u, res)

randomize :: PublicKey -> OT (Integer, Integer)
randomize (g,h,g',h') = do
    p <- gets ot_mod
    s <- randRange (1,p-1)
    t <- randRange (1,p-1)
    u <- bind2 modMult (modExp g  s) (modExp h  t)
    v <- bind2 modMult (modExp g' s) (modExp h' t)
    return (u,v)

ddhDec :: SecretKey -> Ciphertext -> OT Plaintext
ddhDec sk (c0, c1) = do
    p <- gets ot_mod
    modMult c1 =<< (inverseCoprimes <$> modExp c0 sk <*> pure p)

bind2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bind2 f a b = do x <- a; y <- b; f x y
