{-# LANGUAGE NamedFieldPuns #-}

module Crypto.GarbledCircuits.TruthTable
  ( 
    circ2tt
  , circ2tt'
  , evalTT
  )
where

import Crypto.GarbledCircuits.Types
import Crypto.GarbledCircuits.Util hiding (xor)

import qualified Data.Set as S
import qualified Data.Map as M
import           Data.Maybe
import           Control.Monad.State
import           Data.Bits (xor)
import           Data.Functor

--------------------------------------------------------------------------------
-- transform circ to tt

-- NotBinary gets passed around as we transform a Circ to a TruthTable.
data NotBinary = UNot (Ref TruthTable)
               | UConst Bool
               deriving (Eq, Ord, Show)

circ2tt :: Program Circ -> Program TruthTable
circ2tt = fromJust . circ2tt'

circ2tt' :: Program Circ -> Maybe (Program TruthTable)
circ2tt' prog_circ = if success then Just prog_tt else Nothing
  where
    (success, prog_tt) = runState (transform $ prog_output prog_circ) emptyProg

    transform :: [Ref Circ] -> State (Program TruthTable) Bool
    transform outs = do
        eitherOuts <- mapM trans outs
        case mapM check eitherOuts of
          Nothing    -> return False
          Just outs' -> do
              modify (\p -> p { prog_output = outs' })
              return True
      where
        check (Right ref) = Just ref
        check (Left _)    = Nothing

    --return a circ if it is a unary gate in order to fold it into its parent
    trans :: Ref Circ -> State (Program TruthTable) (Either NotBinary (Ref TruthTable))
    trans ref = do
      let circ = lookupC ref prog_circ
      cs <- mapM trans (children circ)
      if boolean circ then do
        let [x,y] = cs
        mkBin (circ2op circ) x y
      else case circ of
        Not _     -> return $ mkNot (head cs)
        Const b   -> return $ Left (UConst b)
        Input i p -> Right <$> inputp p (TTInp i p)
        x         -> error ("[trans] unrecognized pattern: " ++ show x)

--------------------------------------------------------------------------------
-- combine elements - this is the meat of the TruthTable translator

mkBin :: Operation
      -> Either NotBinary (Ref TruthTable)
      -> Either NotBinary (Ref TruthTable)
      -> State (Program TruthTable) (Either NotBinary (Ref TruthTable))

mkBin op (Right x)         (Right y)         = Right <$> internp (create op x y)
mkBin op x                 (Left (UConst b)) = return $ foldConst op b x
mkBin op (Left (UConst b)) y                 = return $ foldConst op b y

mkBin op (Right x)       (Left (UNot y)) = Right <$> internp (create op x y) { tt_negy = True }
mkBin op (Left (UNot x)) (Right y)       = Right <$> internp (create op x y) { tt_negx = True }
mkBin op (Left (UNot x)) (Left (UNot y)) = Right <$> internp (create op x y) { tt_negx = True, tt_negy = True }

mkNot :: Either NotBinary (Ref TruthTable) -> Either NotBinary (Ref TruthTable)
mkNot (Right x)         = Left (UNot x)
mkNot (Left (UNot x))   = Right x
mkNot (Left (UConst b)) = Left (UConst (not b))

create :: Operation -> Ref TruthTable -> Ref TruthTable -> TruthTable
create op x y = emptyTT { tt_f = op,  tt_inpx = x, tt_inpy = y }

foldConst :: Operation -> Bool -> Either NotBinary (Ref TruthTable)
          -> Either NotBinary (Ref TruthTable)
foldConst XOR True  x = mkNot x
foldConst XOR False x = x
foldConst AND True  x = x
foldConst AND False _ = Left (UConst False)
foldConst OR  False x = x
foldConst OR  True  _ = Left (UConst True)
foldConst _ _ _ = err "foldConst" "unrecognized operation"

--------------------------------------------------------------------------------
-- truth table evaluator

evalTT :: [Bool] -> [Bool] -> Program TruthTable -> [Bool]
evalTT inpA inpB prog = evalProg construct prog
  where
    inputs PartyA = M.fromList (zip (S.toList (prog_input_a prog)) inpA)
    inputs PartyB = M.fromList (zip (S.toList (prog_input_b prog)) inpB)

    construct :: Ref TruthTable -> TruthTable -> [Bool] -> Bool
    construct ref (TTInp i p) [] = case M.lookup ref (inputs p) of
        Just b  -> b
        Nothing -> err "construct" ("no input with id: " ++ show i)
    construct _ (TT {tt_f, tt_negx, tt_negy}) [x,y] =
        let x' = if tt_negx then not x else x
            y' = if tt_negy then not y else y
        in eval tt_f x' y'
    construct _ _ _ = err "construct" "bad pattern"

    eval XOR = xor
    eval AND = (&&)
    eval OR  = (||)
    eval op  = err "evalTT" ("unknown binary gate: " ++ show op)

--------------------------------------------------------------------------------
-- helper functions

boolean :: Circ -> Bool
boolean (Xor _ _) = True
boolean (And _ _) = True
boolean (Or  _ _) = True
boolean _ = False
