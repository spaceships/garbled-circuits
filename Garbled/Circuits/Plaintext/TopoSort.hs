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

type Set = S.Set
type DFS = WriterT [CircRef] (State (Set CircRef, Set CircRef))

topoSort :: Program -> [CircRef]
topoSort prog = snd $ evalState (runWriterT loop) initialState
  where
    deref        = env_deref (prog_env prog)
    initialState = (S.fromList (M.keys deref), S.empty)

    look :: CircRef -> Circuit
    look ref = violentLookup ref deref

    loop :: DFS ()
    loop = do
      maybeRef <- next
      case maybeRef of
        Just ref -> visit ref
        Nothing  -> return ()
      
    visit :: CircRef -> DFS ()
    visit ref = do
      let circ = look ref 
      mapM_ visit (circRefs circ)
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

foldConsts :: Program -> Program
foldConsts prog = undefined
  where
    topo = topoSort prog

    rewrite :: CircRef -> Circuit -> State Program ()
    rewrite ref circ = do
      prog <- get
      let env   = prog_env prog
          dedup = M.insert circ ref (env_dedup env)
          deref = M.insert ref circ (env_deref env)
          env'  = env { dedup_env = dedup; deref_env = deref }
          prog' = prog { prog_env = env' }
      put prog'

    fold :: CircRef -> State Program ()
    fold ref = undefined
