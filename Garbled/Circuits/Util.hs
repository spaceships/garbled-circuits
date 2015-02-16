{-# LANGUAGE LambdaCase, ScopedTypeVariables, FlexibleContexts #-}

module Garbled.Circuits.Util
  ( bindM2
  , err
  , word2Bits
  , bits2Word
  , internp
  , inputp
  , writep
  , lookp
  , lookupC
  , topoSort
  , truthVals
  , garbledGate
  )
where

import Garbled.Circuits.Types

import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Bits ((.&.))
import qualified Data.Map as M
import           Data.Word
import qualified Data.Set as S

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
    Nothing  -> error "[lookupRef] no ref"
    Just ref -> ref
  where
    dedup = env_dedup (prog_env prog)

lookupC :: Ref c -> Program c -> c
lookupC ref prog = case M.lookup ref deref of
    Nothing -> error "[lookupC] no c"
    Just c  -> c
  where
    deref = env_deref (prog_env prog)

-- yay polymorphic topoSort
type Set = S.Set
type DFS c = WriterT [Ref c] (State (Set (Ref c), Set (Ref c)))

topoSort :: CanHaveChildren c => Program c -> [Ref c]
topoSort prog = snd $ evalState (runWriterT loop) initialState
  where
    deref        = env_deref (prog_env prog)
    initialState = (S.fromList (M.keys deref), S.empty)
    loop = next >>= \case Just ref -> visit ref; Nothing -> return ()
    visit ref = let circ = lookupC ref prog
                in mapM_ visit (children circ) >> mark ref
    next = gets fst >>= \todo -> return $
      if S.size todo > 0 then Just (S.findMax todo) else Nothing
    mark ref = get >>= \(todo, done) -> do
      put (S.delete ref todo, S.insert ref done)
      tell [ref]

--------------------------------------------------------------------------------
-- garbled gate helpers

garbledGate :: Ref GarbledGate -> Ref GarbledGate -> GarbledGateTable -> GarbledGate
garbledGate x y tab = GarbledGate { gate_inpx  = x
                                  , gate_inpy  = y
                                  , gate_table = tab
                                  }


