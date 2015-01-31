{-# LANGUAGE LambdaCase, ScopedTypeVariables, FlexibleContexts #-}

module Garbled.Circuits.Plaintext.Rewrite
  (
    topoSort
  , foldConsts
  )
where

import Garbled.Circuits.Plaintext.Types
import Garbled.Circuits.Util

import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Set as S
import qualified Data.Map as M
import           Data.Functor
import           Data.Maybe (fromMaybe)

type Set = S.Set

type DFS = WriterT [CircRef] (State (Set CircRef, Set CircRef))

-- yay polymorphic topoSort
topoSort :: (c -> [CircRef]) -> Program c -> [CircRef]
topoSort children prog = snd $ evalState (runWriterT loop) initialState
  where
    deref        = env_deref (prog_env prog)
    initialState = (S.fromList (M.keys deref), S.empty)

    loop :: DFS ()
    loop = do
      maybeRef <- next
      case maybeRef of
        Just ref -> visit ref
        Nothing  -> return ()

    visit :: CircRef -> DFS ()
    visit ref = do
      let circ = fromMaybe (error "[visit] oops") (M.lookup ref deref)
      mapM_ visit (children circ)
      mark ref

    next :: DFS (Maybe CircRef)
    next = do
      (todo, done) <- get
      if S.size todo > 0
        then return $ Just (S.findMax todo)
        else return Nothing

    mark :: CircRef -> DFS ()
    mark ref = do
      (todo, done) <- get
      put (S.delete ref todo, S.insert ref done)
      tell [ref]

foldConsts :: Program Circuit -> Program Circuit
foldConsts prog = execState (mapM_ fold topo) prog
  where
    topo = topoSort circRefs prog

    fold :: CircRef -> State (Program Circuit) ()
    fold ref = do
      circ <- lookp ref
      when (boolean circ) $ do
        [left, right] <- mapM lookp (circRefs circ)
        l <- doFold left
        r <- doFold right
        rewritep ref (circ `withArgs` (l,r))

    getChildren :: Circuit -> State (Program Circuit) [Circuit]
    getChildren circ = mapM lookp (circRefs circ)

    doFold :: Circuit -> State (Program Circuit) (Maybe CircRef)
    doFold c@(Xor x y) = getChildren c >>= \case
      [Const True , Const True ] -> Just <$> internp (Const False)
      [Const False, Const False] -> Just <$> internp (Const False)
      [Const True , _          ] -> Just <$> internp (Not y)
      [Const False, _          ] -> return (Just y)
      [_          , Const True ] -> Just <$> internp (Not x)
      [_          , Const False] -> return (Just x)
      _ -> return Nothing
    doFold c@(And x y) = getChildren c >>= \case
      [Const True , Const True ] -> Just <$> internp (Const True)
      [Const False, Const False] -> Just <$> internp (Const False)
      [Const True , _          ] -> return (Just y)
      [Const False, _          ] -> Just <$> internp (Const False)
      [_          , Const True ] -> return (Just x)
      [_          , Const False] -> Just <$> internp (Const False)
      _ -> return Nothing
    doFold c@(Or x y) = getChildren c >>= \case
      [Const True , Const True ] -> Just <$> internp (Const True)
      [Const False, Const False] -> Just <$> internp (Const False)
      [Const True , _          ] -> Just <$> internp (Const True)
      [Const False, _          ] -> return (Just y)
      [_          , Const True ] -> Just <$> internp (Const True)
      [_          , Const False] -> return (Just x)
      _ -> return Nothing
    doFold _ = return Nothing

--------------------------------------------------------------------------------
-- transform from circ to tt

circ2tt :: Program Circuit -> Program TruthTable
circ2tt prog = execState (mapM transform topo) emptyProgram
  where
    topo = topoSort circRefs prog

    transform :: CircRef -> State (Program TruthTable) ()
    transform ref = do
      let circ = lookupC ref prog
      undefined

--------------------------------------------------------------------------------
-- polymorphic helper functions
    
internp :: (Ord c, MonadState (Program c) m) 
        => c 
        -> m CircRef
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

rewritep :: (Ord c, MonadState (Program c) m)
         => CircRef 
         -> c
         -> m ()
rewritep ref circ = do
  prog <- get
  let env   = prog_env prog
      dedup = M.insert circ ref (env_dedup env)
      deref = M.insert ref circ (env_deref env)
      env'  = env { env_dedup = dedup, env_deref = deref }
  put prog { prog_env = env' }

lookp :: (Ord c, MonadState (Program c) m)
     => CircRef 
     -> m c
lookp ref = do
  env <- gets prog_env
  case M.lookup ref (env_deref env) of
    Nothing -> error "[lookp] no c"
    Just c  -> return c

lookupRef :: Ord c => c -> Program c -> CircRef
lookupRef c prog = case M.lookup c dedup of
    Nothing  -> error "[lookupC] no ref"
    Just ref -> ref
  where 
    dedup = env_dedup (prog_env prog)

lookupC :: CircRef -> Program c -> c
lookupC ref prog = case M.lookup ref deref of
    Nothing -> error "[lookupRef] no c"
    Just c  -> c
  where 
    deref = env_deref (prog_env prog)

--------------------------------------------------------------------------------
-- circuit helper functions

boolean :: Circuit -> Bool
boolean (Xor _ _) = True
boolean (And _ _) = True
boolean (Or  _ _) = True
boolean _ = False

withArgs :: Circuit -> (Maybe CircRef, Maybe CircRef) -> Circuit
withArgs (Xor _ _) (Just x , Just y ) = Xor x y
withArgs (And _ _) (Just x , Just y ) = And x y
withArgs (Or  _ _) (Just x , Just y ) = Or  x y
withArgs (Xor _ y) (Just x , Nothing) = Xor x y
withArgs (And _ y) (Just x , Nothing) = And x y
withArgs (Or  _ y) (Just x , Nothing) = Or  x y
withArgs (Xor x _) (Nothing, Just y ) = Xor x y
withArgs (And x _) (Nothing, Just y ) = And x y
withArgs (Or  x _) (Nothing, Just y ) = Or  x y
withArgs (Xor x y) (Nothing, Nothing) = Xor x y
withArgs (And x y) (Nothing, Nothing) = And x y
withArgs (Or  x y) (Nothing, Nothing) = Or  x y
withArgs x _ = x

