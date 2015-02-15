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

data UnaryOp = UNot (Ref TruthTable)
             | UId  (Ref TruthTable)
             | UConst Bool
             deriving (Eq, Ord, Show)

circ2tt :: Program Circ -> Program TruthTable
circ2tt prog = prog'
  where
    {-topo  = topoSort circRefs prog-}
    prog' = execState (transform $ prog_outputs prog) emptyProg

    transform :: [Ref Circ] -> State (Program TruthTable) ()
    transform outs = mapM_ trans outs

    --return a circ if it is a unary gate in order to fold it into its parent
    trans :: Ref Circ -> State (Program TruthTable) (Either UnaryOp (Ref TruthTable))
    trans ref = do
      let circ = lookupC ref prog
      let op = circ2op circ
      cs <- mapM trans (children circ)
      if boolean circ then do
        let [x,y] = cs
        constructBin (circ2op circ) x y
      else case circ of
        Not _    -> constructNot (head cs)
        Const b  -> return $ Left (UConst b)
        Input id -> Right <$> internp (TTInput id)
        x        -> error ("[trans] unrecognized pattern: " ++ show x)
         
    constructBin :: Operation
                 -> Either UnaryOp (Ref TruthTable) 
                 -> Either UnaryOp (Ref TruthTable) 
                 -> State (Program TruthTable) (Either UnaryOp (Ref TruthTable))
    constructBin op (Right x) (Right y) = Right <$> internp (create op x y)


    -- TODO: DO I REALLY NEED ALL POSSIBLE COMBINATIONS. this is so complicated
    -- UId children: easy
    constructBin op (Left (UId x)) (Right y)      = Right <$> internp (create op x y)
    constructBin op (Right x) (Left (UId y))      = Right <$> internp (create op x y)
    constructBin op (Left (UId x)) (Left (UId y)) = Right <$> internp (create op x y)

    -- UConst: tricky
    constructBin op (Right x) (Left (UConst b)) = return $ Left (foldConst op b x)
    constructBin op (Left (UConst b)) (Right y) = return $ Left (foldConst op b y)
    constructBin op (Left (UConst b1)) (Left (UConst b2)) = 
      return $ Left $ case op of 
        OXor -> UConst $ not (b1 && b2) && not (not b1 && not b2) -- why is XOR not more popular?!?!
        OAnd -> UConst $ b1 && b2
        OOr  -> UConst $ b1 || b2
        _    -> err "constructBin" "unrecognized operation" [op]

    constructBin = fixme

    -- UNot child: tricky

    {-constructBin OAnd (Right x) (Left y) = undefined-}
    {-constructBin OOr  (Right x) (Left y) = undefined-}

    {-constructBin OXor (Left x) (Left y) = undefined-}
    {-constructBin OAnd (Left x) (Left y) = undefined-}
    {-constructBin OOr  (Left x) (Left y) = undefined-}

    {-constructBin op x y = err "constructBin" "unknown pattern" [x,y]-}

    constructNot :: Either UnaryOp (Ref TruthTable)
                 -> State (Program TruthTable) (Either UnaryOp (Ref TruthTable))
    constructNot (Right x)         = return $ Left (UNot x)
    constructNot (Left (UNot x))   = return $ Left (UId x)
    constructNot (Left (UId x))    = return $ Left (UNot x)
    constructNot (Left (UConst b)) = return $ Left (UConst (not b))

    create :: Operation -> Ref TruthTable -> Ref TruthTable -> TruthTable
    create OXor x y = tt_xor { tt_inpx = x, tt_inpy = y }
    create OAnd x y = tt_and { tt_inpx = x, tt_inpy = y }
    create OOr  x y = tt_or  { tt_inpx = x, tt_inpy = y }
    create op x y = err "create" "unrecognized operation" [op]

    foldConst :: Operation -> Bool -> Ref TruthTable -> UnaryOp
    foldConst OXor True  x = UNot x
    foldConst OXor False x = UId x
    foldConst OAnd True  x = UId x
    foldConst OAnd False x = UConst False
    foldConst OOr  True  x = UConst True
    foldConst OOr  False x = UId x
    foldConst op _ _ = err "foldConst" "unrecognized operation" [op]

{-data TruthTable = TruthTable { tt_11   :: Bool-}
                             {-, tt_10   :: Bool-}
                             {-, tt_01   :: Bool-}
                             {-, tt_00   :: Bool-}
                             {-, tt_inpx :: Ref TruthTable-}
                             {-, tt_inpy :: Ref TruthTable-}
                             {-} deriving (Eq, Ord)-}


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

tt_xor = TruthTable { tt_11 = False
                    , tt_10 = True
                    , tt_01 = True
                    , tt_00 = False
                    , tt_inpx = Ref 0
                    , tt_inpy = Ref 0
                    }

tt_and = TruthTable { tt_11 = True
                    , tt_10 = False
                    , tt_01 = False
                    , tt_00 = False
                    , tt_inpx = Ref 0
                    , tt_inpy = Ref 0
                    }

tt_or = TruthTable { tt_11 = True
                   , tt_10 = True
                   , tt_01 = True
                   , tt_00 = False
                   , tt_inpx = Ref 0
                   , tt_inpy = Ref 0
                   }

