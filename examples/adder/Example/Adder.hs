module Example.Adder where

import Crypto.GarbledCircuits
import Crypto.GarbledCircuits.Language as L
import Crypto.GarbledCircuits.Util (bind2, err)

import Control.Monad

--------------------------------------------------------------------------------
-- 8 bit adder example

add1Bit :: Ref Circuit -> Ref Circuit -> Ref Circuit -> Builder (Ref Circuit, Ref Circuit)
add1Bit x y c = do
    s    <- L.xor x y
    out  <- L.xor c s
    cout <- bind2 L.or (L.and x y) (L.and c s)
    return (out, cout)

addBits :: [Ref Circuit] -> [Ref Circuit] -> Builder ([Ref Circuit], Ref Circuit)
addBits xs ys = do
    f <- L.const False
    builder xs ys f []
  where
    builder [] []         c outs = return (outs, c)
    builder (a:as) (b:bs) c outs = do
      (out,c') <- add1Bit a b c
      builder as bs c' (out:outs)
    builder as bs _ _ = err "builder" ("lists of unequal length: " ++ show [as,bs])

adderNBit :: Int -> Program Circuit
adderNBit n = buildCircuit $ do
    inp1      <- replicateM n (input PartyA)
    inp2      <- replicateM n (input PartyB)
    (outs, _) <- addBits inp1 inp2
    return outs

adder8Bit :: Program Circuit
adder8Bit = adderNBit 8
