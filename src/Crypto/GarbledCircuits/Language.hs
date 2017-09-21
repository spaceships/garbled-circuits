{- |
Module      : Crypto.GarbledCircuits.Language
Description : A language for creating boolean circuits.
License     : Apache-2.0
Maintainer  : Brent Carmer <bcarmer@gmail.com>
Stability   : experimental

This module provides a language for creating @Program Circuit@s.

The general idea is that we use the constructors to generate @Ref@s to circuit structures. Then, the
@Builder@ monad makes sure repeated structures get reused.

When you are making a circuit, you can think of a @Ref@ as a circuit's output.

-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Crypto.GarbledCircuits.Language
 -- (
  -- -- * Building and Evaluating Circuits
  --   Builder
  -- , buildCircuit
  -- , evalCircuit
  -- -- * Smart Constructors
  -- -- |Use these constructors to create boolean circuits in the 'Builder' monad.
  -- , input
  -- , xor
  -- , and
  -- , or
  -- , not
  -- , const
  -- )
where

import Prelude hiding (and, not, or, const)
import qualified Prelude

import Crypto.GarbledCircuits.Types
import Crypto.GarbledCircuits.Util hiding (nextRef)

import           Control.Applicative hiding (Const)
import           Control.Monad (zipWithM)
import           Control.Monad.State
import qualified Data.Bits
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Word

data CircuitSt = CircuitSt { st_nextRef     :: Ref Circuit
                           , st_input_gb    :: S.Set (Ref Circuit)
                           , st_input_ev    :: S.Set (Ref Circuit)
                           , st_nextInputId :: InputId
                           , st_deref_env   :: Map (Ref Circuit) Circuit
                           , st_dedup_env   :: Map Circuit (Ref Circuit)
                           }

newtype Builder a = Builder (State CircuitSt a)
                  deriving (Functor, Applicative, Monad, MonadState CircuitSt)

-- |Evaluate a program in plaintext. Useful for testing and debugging.
evalCircuit :: [Bool] -- ^ Garbler's input
            -> [Bool] -- ^ Evaluator's input
            -> Program Circuit  -- ^ The program itself
            -> [Bool] -- ^ The computed output
evalCircuit inpGb inpEv prog = evalProg construct prog
  where
    inputs Garbler   = M.fromList (zip (S.toList (prog_input_gb prog)) inpGb)
    inputs Evaluator = M.fromList (zip (S.toList (prog_input_ev prog)) inpEv)

    construct :: Ref Circuit -> Circuit -> [Bool] -> Bool
    construct ref (Input i p) [] = case M.lookup ref (inputs p) of
      Just b  -> b
      Nothing -> err "reconstruct" ("no input with id " ++ show i)
    construct _ (Const x) []    = x
    construct _ (Not _)   [x]   = Prelude.not x
    construct _ (Xor _ _) [x,y] = Data.Bits.xor x y
    construct _ (And _ _) [x,y] = x && y
    construct _ (Or  _ _) [x,y] = x || y
    construct _ _ _ = err "reconstruct" "unrecognized pattern"

-- |Create a program from smart constructors. Top level 'Ref's are treated as output.
--
-- Note: programs with toplevel 'Not' or 'Const' are not garbleable.
buildCircuit :: Builder [Ref Circuit] -> Program Circuit
buildCircuit (Builder c) = Program { prog_input_gb = st_input_gb st
                                   , prog_input_ev = st_input_ev st
                                   , prog_output  = reverse outs
                                   , prog_env     = st_deref_env st
                                   }
  where
    (outs, st) = runState c emptySt
    emptySt    = CircuitSt { st_nextRef     = Ref 0
                           , st_nextInputId = InputId 0
                           , st_input_gb    = S.empty
                           , st_input_ev    = S.empty
                           , st_deref_env   = emptyEnv
                           , st_dedup_env   = M.empty
                           }

lookupCircuit :: Circuit -> Builder (Maybe (Ref Circuit))
lookupCircuit circ = do
  dedupEnv <- gets st_dedup_env
  return $ M.lookup circ dedupEnv

insertRef :: Ref Circuit -> Circuit -> Builder ()
insertRef ref circ = do
  derefEnv <- gets st_deref_env
  dedupEnv <- gets st_dedup_env
  modify $ \st -> st { st_deref_env = M.insert ref circ derefEnv
                     , st_dedup_env = M.insert circ ref dedupEnv
                     }

nextRef :: Builder (Ref Circuit)
nextRef = do
  ref <- gets st_nextRef
  modify (\st -> st { st_nextRef = succ ref })
  return ref

nextInputId :: Builder InputId
nextInputId = do
  i <- gets st_nextInputId
  modify $ \st -> st { st_nextInputId = succ i }
  return i

intern :: Circuit -> Builder (Ref Circuit)
intern circ = do
  maybeRef <- lookupCircuit circ
  case maybeRef of
    Just ref -> return ref
    Nothing  -> do
      ref <- nextRef
      insertRef ref circ
      return ref

--------------------------------------------------------------------------------
-- smart constructors for the Circuit language

-- |The 'input' function creates an input bit for the 'Garbler' or the 'Evaluator'.
input :: Party -> Builder (Ref Circuit)
input p = do i   <- nextInputId
             ref <- intern (Input i p)
             modify $ \st -> case p of
               Garbler   -> st { st_input_gb = S.insert ref (st_input_gb st) }
               Evaluator -> st { st_input_ev = S.insert ref (st_input_ev st) }
             return ref

inputs :: Int -> Party -> Builder [Ref Circuit]
inputs n p = replicateM n (input p)

-- |Bitwise xor. This gate will be garbled as a 'FreeXor'.
xor :: Ref Circuit -> Ref Circuit -> Builder (Ref Circuit)
xor x y = intern (Xor x y)

-- |Bitwise and. This gate will be garbled as a 'HalfGate'.
and :: Ref Circuit -> Ref Circuit -> Builder (Ref Circuit)
and x y = intern (And x y)

ands :: [Ref Circuit] -> Builder (Ref Circuit)
ands [x]    = return x
ands (x:xs) = and x =<< ands xs

-- |Bitwise or. This gate will be garbled as a 'HalfGate'.
or :: Ref Circuit -> Ref Circuit -> Builder (Ref Circuit)
or x y = intern (Or x y)

-- |Bitwise negation.
-- This gate will be folded into a binary gate above it in the circuit.
--
-- Note: programs with toplevel 'Not' or 'Const' are not garbleable.
not :: Ref Circuit -> Builder (Ref Circuit)
not x = intern (Not x)

-- |Create a constant value.
-- This gate will be folded into a binary gate above it in the circuit.
--
-- Note: programs with toplevel 'Not' or 'Const' are not garbleable.
const :: Bool -> Builder (Ref Circuit)
const b = intern (Const b)

--------------------------------------------------------------------------------
-- high level constructors

ifThenElse :: Ref Circuit -> Ref Circuit -> Ref Circuit -> Builder (Ref Circuit)
ifThenElse cond a b = bind2 or (and cond a) (and b =<< not cond)

-- |Compare two bits.
eqBit :: Ref Circuit -> Ref Circuit -> Builder (Ref Circuit)
eqBit x y = bind2 or (and x y) (bind2 and (not x) (not y))

ltBit :: Ref Circuit -> Ref Circuit -> Builder (Ref Circuit)
ltBit x y = and y =<< not x

-- |Compare two bits.
leqBit :: Ref Circuit -> Ref Circuit -> Builder (Ref Circuit)
leqBit x y = or y =<< not x

-- |Compare two bits.
gtBit :: Ref Circuit -> Ref Circuit -> Builder (Ref Circuit)
gtBit x y = not =<< leqBit x y

-- |Compare two little-endian binary values.
--
-- Note: I do not understand why we have to swap the arguments.
gtBinary :: [Ref Circuit] -> [Ref Circuit] -> Builder (Ref Circuit)
gtBinary xs ys = fst <$> gtHelper ys xs
  where
    gtHelper [x] [y] = do
        gt <- gtBit x y
        eq <- eqBit x y
        return (gt, eq)
    gtHelper (x:xs) (y:ys) = do
        (restGt, restEq) <- gtHelper xs ys
        thisGt <- gtBit x y
        thisEq <- eqBit x y
        gt <- ifThenElse restEq thisGt restGt
        eq <- and restEq thisEq
        return (gt, eq)
    gtHelper _ _ = undefined

