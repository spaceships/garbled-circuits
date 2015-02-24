{-# LANGUAGE LambdaCase, ScopedTypeVariables, FlexibleContexts #-}

module Crypto.GarbledCircuits.Util
  ( bindM2
  , bits2Word
  , err
  , evalProg
  , inputp
  , internp
  , lookp
  , lookupC
  , topoSort
  , truthVals
  , traverse
  , word2Bits
  , writep
  , violentLookup
  )
where

import Crypto.GarbledCircuits.Types

import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Bits
import           Data.Functor
import qualified Data.Map as M
import           Data.Word
import qualified Data.Set as S

--------------------------------------------------------------------------------
-- general helper functions

bindM2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bindM2 f a b = do x <- a; y <- b; f x y

-- returns a little-endian list of bits
word2Bits :: (FiniteBits b, Num b, Ord b, Bits b) => b -> [Bool]
word2Bits x = map (bitAnd x) (take (finiteBitSize x) pow2s)
  where
    bitAnd a b = a .&. b > 0

-- takes a little-endian list of bits
bits2Word :: (Bits a, Num a) => [Bool] -> a
bits2Word bs = sum $ zipWith select bs pow2s
  where
    select b x = if b then x else 0

pow2s :: (Num b, Bits b) => [b]
pow2s = [ shift 1 x | x <- [0..] ]

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
  modify (\p -> p { prog_inputs = S.insert ref (prog_inputs p) })
  return ref

outputp :: (Ord c, MonadState (Program c) m) => Ref c -> m ()
outputp ref = modify (\p -> p { prog_outputs = prog_outputs p ++ [ref] })

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
data DFSSt c = DFSSt { dfs_todo :: Set (Ref c)
                     , dfs_done :: Set (Ref c) 
                     }

type DFS c = WriterT [Ref c] (State (DFSSt c))

-- TODO: this is broken
topoSort :: CanHaveChildren c => Program c -> [Ref c]
topoSort prog = snd $ evalState (runWriterT loop) initialState
  where
    deref = env_deref (prog_env prog)

    initialState = DFSSt { dfs_todo = S.fromList (M.keys deref)
                         , dfs_done = S.empty
                         }

    loop = next >>= \case 
      Just ref -> visit ref
      Nothing  -> return ()

    visit ref = do
      let circ = lookupC ref prog
      mapM_ visit (children circ) 
      mark ref

    next :: DFS c (Maybe (Ref c))
    next = do
      todo <- gets dfs_todo 
      if S.size todo > 0 then 
        return $ Just (S.findMax todo) 
      else 
        return $ Nothing

    mark ref = do
      st <- get
      put st { dfs_todo = S.delete ref (dfs_todo st)
             , dfs_done = S.insert ref (dfs_done st)
             }
      tell [ref]

evalProg :: (Show b, CanHaveChildren c)
         => (c -> [b] -> IO b) -> Program c -> [b] -> IO [b]
evalProg construct prog inps = do
#ifdef DEBUG
    let inputs = zip (map InputId [0..]) inps
    forM inputs $ \(id, inp) -> reportl ("[evalProg] " ++ show id ++ ": " ++ show inp)
#endif
    resultMap <- execStateT (mapM (traverse construct prog) (prog_outputs prog)) M.empty
    let outputs = map (\ref -> (ref, violentLookup resultMap ref)) (prog_outputs prog)
#ifdef DEBUG
    forM outputs $ \(ref, res) -> reportl ("[evalProg] output " ++ show ref ++ ": " ++ show res)
#endif
    return (reverse $ snd <$> outputs)

traverse :: (Show b, MonadState (Map (Ref c) b) m, MonadIO m, CanHaveChildren c) 
         => (c -> [b] -> IO b) -> Program c -> Ref c -> m b
traverse f prog ref = do
  precomputed <- get
  case M.lookup ref precomputed of
    Just b  -> do 
#ifdef DEBUG
      reportl ("[traverse" ++ show ref ++"] precomputed: " ++ show b)
#endif
      return b
    Nothing -> do
      let c = lookupC ref prog
#ifdef DEBUG
      when (length (children c) > 0) $
        reportl ("[traverse" ++ show ref ++"] recursing on children: " ++ show (children c))
#endif
      kids <- mapM (traverse f prog) (children c)
      result <- liftIO $ f c kids
      modify (M.insert ref result)
#ifdef DEBUG
      reportl ("[traverse" ++ show ref ++"] result: " ++ show result)
#endif
      return result

--------------------------------------------------------------------------------
-- evil helpers

reportl :: MonadIO m => String -> m ()
reportl = liftIO . putStrLn

err :: String -> String -> a
err name warning = error $ "[" ++ name ++ "] " ++ warning

violentLookup :: (Show k, Show v, Ord k) => Map k v -> k -> v
violentLookup m k = case M.lookup k m of
  Nothing -> err "violentLookup" ("OOPS: " ++ show m)
  Just  v -> v