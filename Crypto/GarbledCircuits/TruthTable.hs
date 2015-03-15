module Crypto.GarbledCircuits.TruthTable
  ( circ2tt
  , evalTT
  , tableTypes
  )
where

import Crypto.GarbledCircuits.Types
import Crypto.GarbledCircuits.Util (internp, inputp, lookupC, err, evalProg)

import           Data.List (nub)
import qualified Data.Map as M
import qualified Data.Set as S
import           Control.Monad.State
import           Data.Bits (xor)
import           Data.Functor

--------------------------------------------------------------------------------
-- transform circ to tt

-- It is necessary to have a datatype that is neither a TruthTable nor a Circ,
-- since there is not a 1-to-1 correspondence between Nullary/Unary/Binary gates
-- and Binary gates. NotBinary gets passed around as we transform a Circ to a
-- TruthTable.
data NotBinary = UNot (Ref TruthTable)
               | UConst Bool
               deriving (Eq, Ord, Show)

circ2tt :: Program Circ -> Maybe (Program TruthTable)
circ2tt prog_circ = if success then Just prog_tt else Nothing
  where
    (success, prog_tt) = runState (transform $ prog_outputs prog_circ) emptyProg

    transform :: [Ref Circ] -> State (Program TruthTable) Bool
    transform outs = do
        mapM_ trans (S.toList $ prog_inputs prog_circ)
        eitherOuts <- mapM trans outs
        case mapM check eitherOuts of
          Nothing    -> return False
          Just outs' -> do 
              modify (\p -> p { prog_outputs = outs' })
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
        Not _   -> return $ mkNot (head cs)
        Const b -> return $ Left (UConst b)
        Input i -> Right <$> inputp (TTInp i)
        x       -> error ("[trans] unrecognized pattern: " ++ show x)

--------------------------------------------------------------------------------
-- combine elements - this is the meat of the TruthTable translator

mkBin :: Operation
      -> Either NotBinary (Ref TruthTable)
      -> Either NotBinary (Ref TruthTable)
      -> State (Program TruthTable) (Either NotBinary (Ref TruthTable))

mkBin op (Right x)         (Right y)         = Right <$> internp (create op x y)
mkBin op x                 (Left (UConst b)) = return $ foldConst op b x
mkBin op (Left (UConst b)) y                 = return $ foldConst op b y

mkBin op (Right x)       (Left (UNot y)) = Right <$> internp (flipYs (create op x y))
mkBin op (Left (UNot x)) (Right y)       = Right <$> internp (flipXs (create op x y))
mkBin op (Left (UNot x)) (Left (UNot y)) = Right <$> internp (flipYs (flipXs (create op x y)))

mkNot :: Either NotBinary (Ref TruthTable) -> Either NotBinary (Ref TruthTable)
mkNot (Right x)         = Left (UNot x)
mkNot (Left (UNot x))   = Right x
mkNot (Left (UConst b)) = Left (UConst (not b))

create :: Operation -> Ref TruthTable -> Ref TruthTable -> TruthTable
create OXor x y = TT { tt_f = xor,  tt_inpx = x, tt_inpy = y }
create OAnd x y = TT { tt_f = (&&), tt_inpx = x, tt_inpy = y }
create op   _ _ = err "create" ("unrecognized operation" ++ show op)

foldConst :: Operation -> Bool -> Either NotBinary (Ref TruthTable) 
          -> Either NotBinary (Ref TruthTable)
foldConst OXor True  x = mkNot x
foldConst OXor False x = x
foldConst OAnd True  x = x
foldConst OAnd False _ = Left (UConst False)
foldConst _ _ _ = err "foldConst" "unrecognized operation"

--------------------------------------------------------------------------------
-- truth table evaluator

evalTT :: [Bool] -> Maybe (Program TruthTable) -> [Bool]
evalTT _    Nothing     = err "evalTT" "Recieved failed Program TruthTable"
evalTT inps (Just prog) = evalProg reconstruct prog
  where
    inputs = M.fromList (zip (map InputId [0..]) inps)

    reconstruct :: Ref TruthTable -> TruthTable -> [Bool] -> Bool
    reconstruct _ (TTInp i) [] = case M.lookup i inputs of
      Just b  -> b
      Nothing -> err "reconstruct" ("no input with id: " ++ show i)
    reconstruct _ (TT {tt_f = f}) [x,y] = f x y
    reconstruct _ _ _ = err "reconstruct" "bad pattern"

--------------------------------------------------------------------------------
-- helper functions

flipYs :: TruthTable -> TruthTable
flipYs (TTInp i) = TTInp i
flipYs tt = tt { tt_f = \x y -> tt_f tt x (not y) }

flipXs :: TruthTable -> TruthTable
flipXs (TTInp i) = TTInp i
flipXs tt = tt { tt_f = \x y -> tt_f tt (not x) y }

boolean :: Circ -> Bool
boolean (Xor _ _) = True
boolean (And _ _) = True
boolean _ = False

tableTypes :: Program TruthTable -> [String]
tableTypes prog = nub (map show (filter isGate elems))
  where
    isGate (TTInp _) = False
    isGate _ = True
    elems = M.keys (env_dedup (prog_env prog))
