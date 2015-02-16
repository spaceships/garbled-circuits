{-# LANGUAGE LambdaCase #-}


module Garbled.Circuits.Plaintext.Rewrite
  (
    topoSort
  , circ2tt
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
    loop = next >>= \case Just ref -> visit ref; Nothing -> return ()
    visit ref = let circ = lookupC ref prog 
                in mapM_ visit (children circ) >> mark ref
    next = gets fst >>= \todo -> return $ 
      if S.size todo > 0 then Just (S.findMax todo) else Nothing
    mark ref = get >>= \(todo, done) -> do
      put (S.delete ref todo, S.insert ref done) 
      tell [ref]

--------------------------------------------------------------------------------
-- transform from circ to tt

data UnaryOp = UNot (Ref TruthTable)
             | UId  (Ref TruthTable)
             | UConst Bool
             deriving (Eq, Ord, Show)

circ2tt :: Program Circ -> Program TruthTable
circ2tt prog = prog'
  where
    prog' = execState (transform $ prog_outputs prog) emptyProg

    transform :: [Ref Circ] -> State (Program TruthTable) ()
    transform outs = do 
      eitherOuts <- mapM trans outs
      let check = either (\x -> err "transform" "non binary top level gate" [x]) id
          outs' = map check eitherOuts
      modify (\p -> p { prog_outputs = outs' })

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
        Input id -> Right <$> inputp (TTInp id)
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
        OXor -> UConst $ b1 `xor` b2
        OAnd -> UConst $ b1 && b2
        OOr  -> UConst $ b1 || b2
        _    -> err "constructBin" "unrecognized operation" [op]
    -- UNot child: tricky
    constructBin op (Right x) (Left (UNot y)) = Right <$> internp (flipYs (create op x y))
    constructBin op (Left (UNot x)) (Right y) = Right <$> internp (flipXs (create op x y))
    constructBin op (Left (UNot x)) (Left (UNot y)) =
      Right <$> internp (flipYs (flipXs (create op x y)))

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

--------------------------------------------------------------------------------
-- circuit helper functions

boolean :: Circ -> Bool
boolean (Xor _ _) = True
boolean (And _ _) = True
boolean (Or  _ _) = True
boolean _ = False
