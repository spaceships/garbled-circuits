{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

module Garbled.Circuits.Util where

import Garbled.Circuits.Types

import           Control.Monad.State
import           Data.Bits
import qualified Data.Map as M
import           Data.Word

bindM2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bindM2 f a b = do x <- a; y <- b; f x y

err :: Show s => String -> String -> [s] -> a
err name warning xs = error $ "[" ++ name ++ "] " ++ warning ++ ": " ++ unwords (map show xs)

violentLookup r e = case M.lookup r e of
  Nothing -> error "[violentLookup] something went horribly wrong"
  Just x  -> x

-- returns a little-endian list of bits
word2Bits :: Word8 -> [Bool]
word2Bits x = map (bitAnd x) (take 8 pow2s)
  where
    bitAnd a b = a .&. b > 0

-- takes a little-endian list of bits
{-bits2Word :: Integral n => [Bool] -> n-}
bits2Word bs = sum $ zipWith select bs pow2s
  where
    select b x = if b then x else 0

{-pow2s :: Num a => [a]-}
pow2s = [ 2 ^ x | x <- [0..] ]

emptyProg :: Program c
emptyProg = Program { prog_inputs = [], prog_outputs = [], prog_env = emptyEnv }

emptyEnv :: Env c
emptyEnv = Env { env_deref = M.empty, env_dedup = M.empty }

--------------------------------------------------------------------------------
-- polymorphic helper functions
    
internp :: (Ord c, MonadState (Program c) m) 
        => c 
        -> m (Ref c)
internp circ = do
  prog <- get
  let env   = prog_env prog
      dedup = env_dedup env
      deref = env_deref env
  case M.lookup circ dedup of
    Just ref -> return ref
    Nothing  -> do
      let ref = fst $ M.findMax deref
          dedup' = M.insert circ ref dedup
          deref' = M.insert ref circ deref
          env'   = env { env_dedup = dedup, env_deref = deref }
      put prog { prog_env = env' }
      return ref

writep :: (Ord c, MonadState (Program c) m)
         => Ref c
         -> c
         -> m ()
writep ref circ = do
  prog <- get
  let env   = prog_env prog
      dedup = M.insert circ ref (env_dedup env)
      deref = M.insert ref circ (env_deref env)
      env'  = env { env_dedup = dedup, env_deref = deref }
  put prog { prog_env = env' }

lookp :: (Ord c, MonadState (Program c) m)
     => Ref c
     -> m c
lookp ref = do
  env <- gets prog_env
  case M.lookup ref (env_deref env) of
    Nothing -> error "[lookp] no c"
    Just c  -> return c

lookupRef :: Ord c => c -> Program c -> Ref c
lookupRef c prog = case M.lookup c dedup of
    Nothing  -> error "[lookupC] no ref"
    Just ref -> ref
  where 
    dedup = env_dedup (prog_env prog)

lookupC :: Ref c -> Program c -> c
lookupC ref prog = case M.lookup ref deref of
    Nothing -> error "[lookupRef] no c"
    Just c  -> c
  where 
    deref = env_deref (prog_env prog)

