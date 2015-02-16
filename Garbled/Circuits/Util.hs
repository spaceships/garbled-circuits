{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

module Garbled.Circuits.Util where

import Garbled.Circuits.Types

import           Control.Monad.State
import           Data.Bits ((.&.))
import qualified Data.Map as M
import           Data.Word

--------------------------------------------------------------------------------
-- general helper functions

bindM2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bindM2 f a b = do x <- a; y <- b; f x y

err :: Show s => String -> String -> [s] -> a
err name warning xs = error $ "[" ++ name ++ "] " ++ warning ++ ": " ++ unwords (map show xs)

-- returns a little-endian list of bits
word2Bits :: Word8 -> [Bool]
word2Bits x = map (bitAnd x) (take 8 pow2s)
  where
    bitAnd a b = a .&. b > 0

-- takes a little-endian list of bits
bits2Word bs = sum $ zipWith select bs pow2s
  where
    select b x = if b then x else 0

pow2s = [ 2 ^ x | x <- [0..] ]

xor :: Bool -> Bool -> Bool
xor x y = not (x && y) && not (not x && not y)

--------------------------------------------------------------------------------
-- polymorphic helper functions for State monads over a Program

internp :: (Ord c, MonadState (Program c) m) => c -> m (Ref c)
internp circ = do
  prog <- get
  let env   = prog_env prog
      dedup = env_dedup env
      deref = env_deref env
  case M.lookup circ dedup of
    Just ref -> return ref
    Nothing  -> do
      let ref    = if M.null deref then Ref 0 else succ $ fst (M.findMax deref)
          dedup' = M.insert circ ref dedup
          deref' = M.insert ref circ deref
          env'   = env { env_dedup = dedup', env_deref = deref' }
      put prog { prog_env = env' }
      return ref

inputp :: (Ord c, MonadState (Program c) m) => c -> m (Ref c)
inputp inp = do
  ref <- internp inp
  modify (\p -> p { prog_inputs = ref : prog_inputs p })
  return ref

outputp :: (Ord c, MonadState (Program c) m) => Ref c -> m ()
outputp ref = modify (\p -> p { prog_outputs = ref : prog_outputs p })

writep :: (Ord c, MonadState (Program c) m) => Ref c -> c -> m ()
writep ref circ = do
  prog <- get
  let env   = prog_env prog
      dedup = M.insert circ ref (env_dedup env)
      deref = M.insert ref circ (env_deref env)
      env'  = env { env_dedup = dedup, env_deref = deref }
  put prog { prog_env = env' }

lookp :: (Ord c, MonadState (Program c) m) => Ref c -> m c
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

--------------------------------------------------------------------------------
-- truth table helper functions

flipYs :: TruthTable -> TruthTable
flipYs (TTInp id) = TTInp id
flipYs tt = tt { tt_f = \x y -> tt_f tt x (not y) }

flipXs :: TruthTable -> TruthTable
flipXs (TTInp id) = TTInp id
flipXs tt = tt { tt_f = \x y -> tt_f tt (not x) y }

tt_xor = TT { tt_f = xor,  tt_inpx = undefined, tt_inpy = undefined }
tt_and = TT { tt_f = (&&), tt_inpx = undefined, tt_inpy = undefined }
tt_or  = TT { tt_f = (||), tt_inpx = undefined, tt_inpy = undefined }

--------------------------------------------------------------------------------
-- circuit helper functions

boolean :: Circ -> Bool
boolean (Xor _ _) = True
boolean (And _ _) = True
boolean (Or  _ _) = True
boolean _ = False
