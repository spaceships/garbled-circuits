{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Garbled.Circuits where

import qualified Data.Map as M
import           Control.Monad.State
import           Prelude hiding (or, and)

type Map = M.Map

-- circuit language
-- garbled circuit representation
-- garbling
-- ot
-- evaluation

newtype Ref     = Ref     Int deriving (Enum, Ord, Eq, Show)
newtype InputId = InputId Int deriving (Enum, Ord, Eq, Show)

data Circ = Input InputId
          | Const Bool
          | Not Ref
          | Xor Ref Ref
          | And Ref Ref
          | Or  Ref Ref
          deriving (Show, Eq, Ord)

data Env = Env { env_deref :: Map Ref Circ
               , env_dedup :: Map Circ Ref 
               } deriving (Show)

data CircSt = CircSt { st_nextRef     :: Ref
                     , st_nextInputId :: InputId
                     , st_env         :: Env
                     } deriving (Show)

type CircBuilder a = State CircSt a

buildCirc :: CircBuilder a -> (a, Env)
buildCirc c = let (out, st) = runState c emptySt in (out, st_env st)
  where
    emptySt = CircSt { st_nextRef     = Ref 0
                     , st_nextInputId = InputId 0
                     , st_env         = Env M.empty M.empty
                     }

lookupCirc :: Circ -> CircBuilder (Maybe Ref)
lookupCirc circ = do
  dedupEnv <- gets (env_dedup . st_env)
  return (M.lookup circ dedupEnv)

lookupRef :: Ref -> CircBuilder (Maybe Circ)
lookupRef ref = do
  derefEnv <- gets (env_deref . st_env)
  return (M.lookup ref derefEnv)

-- for internal use only. use intern instead.
insertRef :: Ref -> Circ -> CircBuilder ()
insertRef ref circ = do
  derefEnv <- gets (env_deref . st_env)
  dedupEnv <- gets (env_dedup . st_env)
  modify (\st -> st { st_env = 
    Env (M.insert ref circ derefEnv)
        (M.insert circ ref dedupEnv)
    })
    

nextRef :: CircBuilder Ref
nextRef = do
  ref <- gets st_nextRef
  modify (\st -> st { st_nextRef = succ ref })
  return ref

nextInputId :: CircBuilder InputId
nextInputId = do
  id <- gets st_nextInputId
  modify (\st -> st { st_nextInputId = succ id })
  return id

-- general use
intern :: Circ -> CircBuilder Ref
intern circ = do
  maybeRef <- lookupCirc circ
  case maybeRef of
    Just ref -> return ref
    Nothing  -> do
      ref <- nextRef
      insertRef ref circ
      return ref

input :: CircBuilder Ref
input = do
  id <- nextInputId
  intern (Input id)

xor :: Ref -> Ref -> CircBuilder Ref
xor x y = intern (Xor x y)

or :: Ref -> Ref -> CircBuilder Ref
or x y = intern (Or x y)

and :: Ref -> Ref -> CircBuilder Ref
and x y = intern (And x y)

constant :: Bool -> CircBuilder Ref
constant b = intern (Const b)

add1Bit :: Ref -> Ref -> Ref -> CircBuilder (Ref, Ref)
add1Bit x y c = do
    s    <- xor x y
    out  <- xor c s
    cout <- bindM2 or (and x y) (and c s)
    return (out, cout)

addBits :: [Ref] -> [Ref] -> CircBuilder ([Ref], Ref)
addBits xs ys = do
    f <- constant False
    builder xs ys f []
  where
    builder [] []         c outs = return (outs, c)
    builder (x:xs) (y:ys) c outs = do 
      (out,c') <- add1Bit x y c
      builder xs ys c' (out:outs)

circ_8BitAdder :: CircBuilder [Ref]
circ_8BitAdder = do
    inp1      <- replicateM 8 input
    inp2      <- replicateM 8 input
    (outs, _) <- addBits inp1 inp2
    return outs

bindM2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bindM2 m a b = do x <- a; y <- b; m x y
