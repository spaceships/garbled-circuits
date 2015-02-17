module Garbled.Circuits.Plaintext.TruthTable
  ( circ2tt
  , evalTT
  )
where

import Garbled.Circuits.Types
import Garbled.Circuits.Util (internp, inputp, lookupC, err, evalProg)

import qualified Data.Map as M
import           Control.Monad.State
import           Data.Bits (xor)
import           Data.Functor

--------------------------------------------------------------------------------
-- transform circ to tt

-- it is convenient to have a Circ without associated data
data Operation = OInput
               | OConst
               | ONot
               | OXor
               | OAnd
               | OOr
               deriving (Show, Eq, Ord)

data NotBinary = UNot (Ref TruthTable)
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
        let outs' = map check eitherOuts
        modify (\p -> p { prog_outputs = outs' })
      where
        check (Right ref)      = ref
        check (Left (UId ref)) = ref
        check (Left x)         = err "check" "non binary top level gate" [x]

    --return a circ if it is a unary gate in order to fold it into its parent
    trans :: Ref Circ -> State (Program TruthTable) (Either NotBinary (Ref TruthTable))
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
                 -> Either NotBinary (Ref TruthTable)
                 -> Either NotBinary (Ref TruthTable)
                 -> State (Program TruthTable) (Either NotBinary (Ref TruthTable))
    constructBin op (Right x) (Right y) = Right <$> internp (create op x y)
    -- TODO: DO I REALLY NEED ALL POSSIBLE COMBINATIONS. this is so complicated
    -- UId children: easy
    constructBin op (Left (UId x)) (Right y)      = Right <$> internp (create op x y)
    constructBin op (Right x) (Left (UId y))      = Right <$> internp (create op x y)
    constructBin op (Left (UId x)) (Left (UId y)) = Right <$> internp (create op x y)
    -- UConst children: tricky
    constructBin op (Right x) (Left (UConst b)) = return $ Left (foldConst op b x)
    constructBin op (Left (UConst b)) (Right y) = return $ Left (foldConst op b y)
    constructBin op (Left (UConst b1)) (Left (UConst b2)) =
      return $ Left $ case op of
        OXor -> UConst $ b1 `xor` b2
        OAnd -> UConst $ b1 && b2
        OOr  -> UConst $ b1 || b2
        _    -> err "constructBin" "unrecognized operation" [op]
    -- UNot children: tricky
    constructBin op (Right x) (Left (UNot y)) = Right <$> internp (flipYs (create op x y))
    constructBin op (Left (UNot x)) (Right y) = Right <$> internp (flipXs (create op x y))
    constructBin op (Left (UNot x)) (Left (UNot y)) =
      Right <$> internp (flipYs (flipXs (create op x y)))

    constructNot :: Either NotBinary (Ref TruthTable)
                 -> State (Program TruthTable) (Either NotBinary (Ref TruthTable))
    constructNot (Right x)         = return $ Left (UNot x)
    constructNot (Left (UNot x))   = return $ Left (UId x)
    constructNot (Left (UId x))    = return $ Left (UNot x)
    constructNot (Left (UConst b)) = return $ Left (UConst (not b))

    create :: Operation -> Ref TruthTable -> Ref TruthTable -> TruthTable
    create OXor x y = tt_xor { tt_inpx = x, tt_inpy = y }
    create OAnd x y = tt_and { tt_inpx = x, tt_inpy = y }
    create OOr  x y = tt_or  { tt_inpx = x, tt_inpy = y }
    create op x y = err "create" "unrecognized operation" [op]

    foldConst :: Operation -> Bool -> Ref TruthTable -> NotBinary
    foldConst OXor True  x = UNot x
    foldConst OXor False x = UId x
    foldConst OAnd True  x = UId x
    foldConst OAnd False x = UConst False
    foldConst OOr  True  x = UConst True
    foldConst OOr  False x = UId x
    foldConst op _ _ = err "foldConst" "unrecognized operation" [op]

--------------------------------------------------------------------------------
-- truth table evaluator

evalTT :: Program TruthTable -> [Bool] -> [Bool]
evalTT prog inps = evalProg reconstruct prog inps
  where
    inputs = M.fromList (zip (map InputId [0..]) inps)

    reconstruct :: TruthTable -> [Bool] -> Bool
    reconstruct (TTInp id) [] = case M.lookup id inputs of
      Just b  -> b
      Nothing -> err "reconstruct" "no input with id" [id]
    reconstruct (TT {tt_f = f}) [x,y] = f x y
    reconstruct _ _ = err "reconstruct" "bad pattern" [-1]

--------------------------------------------------------------------------------
-- helper functions

flipYs :: TruthTable -> TruthTable
flipYs (TTInp id) = TTInp id
flipYs tt = tt { tt_f = \x y -> tt_f tt x (not y) }

flipXs :: TruthTable -> TruthTable
flipXs (TTInp id) = TTInp id
flipXs tt = tt { tt_f = \x y -> tt_f tt (not x) y }

tt_xor = TT { tt_f = xor,  tt_inpx = undefined, tt_inpy = undefined }
tt_and = TT { tt_f = (&&), tt_inpx = undefined, tt_inpy = undefined }
tt_or  = TT { tt_f = (||), tt_inpx = undefined, tt_inpy = undefined }

boolean :: Circ -> Bool
boolean (Xor _ _) = True
boolean (And _ _) = True
boolean (Or  _ _) = True
boolean _ = False

circ2op :: Circ -> Operation
circ2op (Input _) = OInput
circ2op (Const _) = OConst
circ2op (Not   _) = ONot
circ2op (Xor _ _) = OXor
circ2op (And _ _) = OAnd
circ2op (Or  _ _) = OOr
