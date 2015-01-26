module Garbled.Circuits.Util where

import qualified Data.Map as M

bindM2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bindM2 f a b = do x <- a; y <- b; f x y

violentLookup r e = case M.lookup r e of
  Nothing -> error "[violentLookup] something went horribly wrong"
  Just x  -> x

