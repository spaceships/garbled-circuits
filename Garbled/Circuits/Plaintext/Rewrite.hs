{-# LANGUAGE LambdaCase #-}


module Garbled.Circuits.Plaintext.Rewrite
  (
    topoSort
  {-, foldConsts-}
  )
where

import Garbled.Circuits.Types
import Garbled.Circuits.Util

import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Functor
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import qualified Data.Set as S

type Set = S.Set

type DFS c = WriterT [Ref c] (State (Set (Ref c), Set (Ref c)))

-- yay polymorphic topoSort
topoSort :: CanHaveChildren c => Program c -> [Ref c]
topoSort prog = snd $ evalState (runWriterT loop) initialState
  where
    deref        = env_deref (prog_env prog)
    initialState = (S.fromList (M.keys deref), S.empty)

    loop :: CanHaveChildren c => DFS c ()
    loop = do
      maybeRef <- next
      case maybeRef of
        Just ref -> visit ref
        Nothing  -> return ()

    visit :: CanHaveChildren c => Ref c -> DFS c ()
    visit ref = do
      {-let circ = fromMaybe (error "[visit] oops") (M.lookup ref deref)-}
      undefined
      {-mapM_ visit (children circ)-}
      {-mark ref-}

    next :: CanHaveChildren c => DFS c (Maybe (Ref c))
    next = do
      (todo, done) <- get
      if S.size todo > 0
        then return $ Just (S.findMax todo)
        else return Nothing

    mark :: CanHaveChildren c => Ref c -> DFS c ()
    mark ref = do
      (todo, done) <- get
      put (S.delete ref todo, S.insert ref done)
      tell [ref]

{-foldConsts :: Program Circ -> Program Circ-}
{-foldConsts prog = execState (mapM_ fold topo) prog-}
  {-where-}
    {-topo = topoSort circRefs prog-}

    {-fold :: Ref -> State (Program Circ) ()-}
    {-fold ref = do-}
      {-circ <- lookp ref-}
      {-when (boolean circ) $ do-}
        {-[left, right] <- mapM lookp (circRefs circ)-}
        {-l <- doFold left-}
        {-r <- doFold right-}
        {-writep ref (circ `withArgs` (l,r))-}

    {-getChildren :: Circ -> State (Program Circ) [Circ]-}
    {-getChildren circ = mapM lookp (circRefs circ)-}

    {-doFold :: Circ -> State (Program Circ) (Maybe Ref)-}
    {-doFold c@(Xor x y) = getChildren c >>= \case-}
      {-[Const True , Const True ] -> Just <$> internp (Const False)-}
      {-[Const False, Const False] -> Just <$> internp (Const False)-}
      {-[Const True , _          ] -> Just <$> internp (Not y)-}
      {-[Const False, _          ] -> return (Just y)-}
      {-[_          , Const True ] -> Just <$> internp (Not x)-}
      {-[_          , Const False] -> return (Just x)-}
      {-_ -> return Nothing-}
    {-doFold c@(And x y) = getChildren c >>= \case-}
      {-[Const True , Const True ] -> Just <$> internp (Const True)-}
      {-[Const False, Const False] -> Just <$> internp (Const False)-}
      {-[Const True , _          ] -> return (Just y)-}
      {-[Const False, _          ] -> Just <$> internp (Const False)-}
      {-[_          , Const True ] -> return (Just x)-}
      {-[_          , Const False] -> Just <$> internp (Const False)-}
      {-_ -> return Nothing-}
    {-doFold c@(Or x y) = getChildren c >>= \case-}
      {-[Const True , Const True ] -> Just <$> internp (Const True)-}
      {-[Const False, Const False] -> Just <$> internp (Const False)-}
      {-[Const True , _          ] -> Just <$> internp (Const True)-}
      {-[Const False, _          ] -> return (Just y)-}
      {-[_          , Const True ] -> Just <$> internp (Const True)-}
      {-[_          , Const False] -> return (Just x)-}
      {-_ -> return Nothing-}
    {-doFold _ = return Nothing-}

--------------------------------------------------------------------------------
-- transform from circ to tt

circ2tt :: Program Circ -> Program TruthTable
circ2tt prog = prog'
  where
    {-topo  = topoSort circRefs prog-}
    prog' = execState (transform $ prog_outputs prog) emptyProg

    transform :: [Ref Circ] -> State (Program TruthTable) ()
    transform = undefined

    {-transform :: Ref -> State (Program TruthTable) (Either Circ Ref)-}
    {-transform ref = do-}
      {-let circ = lookupC ref prog-}
      {-if boolean circ then do-}
        {-undefined-}
        {-[>case circ of<]-}
          {-[>Xor x y -> TruthTable { tt_11 = <]-}
      {-else do-}
        {--- unary gates-}
        {-undefined-}
      

--------------------------------------------------------------------------------
-- circuit helper functions

boolean :: Circ -> Bool
boolean (Xor _ _) = True
boolean (And _ _) = True
boolean (Or  _ _) = True
boolean _ = False

{-withArgs :: Circ -> (Maybe (Ref Circ), Maybe Ref) -> Circ-}
{-withArgs (Xor _ _) (Just x , Just y ) = Xor x y-}
{-withArgs (And _ _) (Just x , Just y ) = And x y-}
{-withArgs (Or  _ _) (Just x , Just y ) = Or  x y-}
{-withArgs (Xor _ y) (Just x , Nothing) = Xor x y-}
{-withArgs (And _ y) (Just x , Nothing) = And x y-}
{-withArgs (Or  _ y) (Just x , Nothing) = Or  x y-}
{-withArgs (Xor x _) (Nothing, Just y ) = Xor x y-}
{-withArgs (And x _) (Nothing, Just y ) = And x y-}
{-withArgs (Or  x _) (Nothing, Just y ) = Or  x y-}
{-withArgs (Xor x y) (Nothing, Nothing) = Xor x y-}
{-withArgs (And x y) (Nothing, Nothing) = And x y-}
{-withArgs (Or  x y) (Nothing, Nothing) = Or  x y-}
{-withArgs x _ = x-}

