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
  ( 
  -- * Building and Evaluating Circuits
    Builder
  , buildCircuit
  , evalCircuit
  -- * Smart Constructors
  -- |Use these constructors to create boolean circuits in the 'Builder' monad.
  , input
  , xor
  , and
  , or
  , not
  , const
  )
where

import Prelude hiding (and, not, or, const)
import qualified Prelude

import Crypto.GarbledCircuits.Types
import Crypto.GarbledCircuits.Util hiding (nextRef)

import           Control.Applicative hiding (Const)
import           Control.Monad.State
import qualified Data.Bits
import qualified Data.Map as M
import qualified Data.Set as S

data CircuitSt = CircuitSt { st_nextRef     :: Ref Circuit
                           , st_inputs_a    :: S.Set (Ref Circuit)
                           , st_inputs_b    :: S.Set (Ref Circuit)
                           , st_nextInputId :: InputId
                           , st_deref_env   :: Map (Ref Circuit) Circuit
                           , st_dedup_env   :: Map Circuit (Ref Circuit)
                           }

newtype Builder a = Builder (State CircuitSt a)
                  deriving (Functor, Applicative, Monad, MonadState CircuitSt)

-- |Evaluate a program in plaintext. Useful for testing and debugging.
evalCircuit :: [Bool] -- ^ PartyA's input
            -> [Bool] -- ^ PartyB's input
            -> Program Circuit  -- ^ The program itself
            -> [Bool] -- ^ The computed output
evalCircuit inpA inpB prog = evalProg construct prog
  where
    inputs PartyA = M.fromList (zip (S.toList (prog_input_a prog)) inpA)
    inputs PartyB = M.fromList (zip (S.toList (prog_input_b prog)) inpB)

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
buildCircuit (Builder c) = Program { prog_input_a = st_inputs_a st
                         , prog_input_b = st_inputs_b st
                         , prog_output  = reverse outs
                         , prog_env     = st_deref_env st
                         }
  where
    (outs, st) = runState c emptySt
    emptySt    = CircuitSt { st_nextRef     = Ref 0
                           , st_nextInputId = InputId 0
                           , st_inputs_a    = S.empty
                           , st_inputs_b    = S.empty
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

-- |The 'input' function creates an input bit for 'PartyA' or 'PartyB'.
input :: Party -> Builder (Ref Circuit)
input p = do i   <- nextInputId
             ref <- intern (Input i p)
             modify $ \st -> case p of
               PartyA -> st { st_inputs_a = S.insert ref (st_inputs_a st) }
               PartyB -> st { st_inputs_b = S.insert ref (st_inputs_b st) }
             return ref

-- |Bitwise xor. This gate will be garbled as a 'FreeXor'.
xor :: Ref Circuit -> Ref Circuit -> Builder (Ref Circuit)
xor x y = intern (Xor x y)

-- |Bitwise and. This gate will be garbled as a 'HalfGate'.
and :: Ref Circuit -> Ref Circuit -> Builder (Ref Circuit)
and x y = intern (And x y)

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
