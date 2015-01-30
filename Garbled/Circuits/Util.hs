module Garbled.Circuits.Util where

import qualified Data.Map as M

import Data.Word
import Data.Bits

bindM2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bindM2 f a b = do x <- a; y <- b; f x y

violentLookup r e = case M.lookup r e of
  Nothing -> error "[violentLookup] something went horribly wrong"
  Just x  -> x

word2Bits :: Word8 -> [Bool]
word2Bits x = reverse $ map (bitAnd x) (take 8 pow2s)
  where
    bitAnd a b = a .&. b > 0

pow2s :: Num a => [a]
pow2s = [ 2 ^ x | x <- [0..] ]
