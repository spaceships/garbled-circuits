module Crypto.GarbledCircuits.Evaluator
  (
    evalLocal
  )
where

import Crypto.GarbledCircuits.Encryption
import Crypto.GarbledCircuits.Types
import Crypto.GarbledCircuits.Util

import           Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as Set

#ifdef DEBUG
import Debug.Trace
#endif

--------------------------------------------------------------------------------
-- garbled evaluator

-- evaluate a garbled circuit locally
evalLocal :: [Bool] -> (Program GarbledGate, Context) -> [Bool]
evalLocal inps (prog, ctx) =
#ifdef DEBUG
    trace (showGG prog ++ showPairs ctx) $
#endif
    map ungarble result
  where
    resultMap = execState (traverseGG reconstruct prog 0) M.empty
    result    = reverse $ map (resultMap !) (prog_outputs prog)

    inpwlps  = map (violentLookup $ ctx_pairs ctx) (Set.toList $ prog_inputs prog)
    inpwires = zipWith sel inps inpwlps
    inputs   = zip (map InputId [0..]) inpwires

    reconstruct :: Ref GarbledGate -> GarbledGate -> [Wirelabel] -> State Int Wirelabel

    reconstruct _ (GarbledInput i) [] = case lookup i inputs of
      Nothing -> err "reconstruct" ("no input wire with id " ++ show i ++ "\n" ++ show inputs)
      Just wl -> return wl

    reconstruct _ (FreeXor _ _) [a,b] = return $ check (a `xor` b)

    reconstruct _ (HalfGate _ _ g e) [a,b] = do
        j1 <- nextIndex
        j2 <- nextIndex
        let wg  = hash (ctx_key ctx) a j1 `xor` (lsb a `mask` g)
            we  = hash (ctx_key ctx) b j2 `xor` (lsb b `mask` (e `xor` a))
        return $ check (wg `xor` we)

    reconstruct _ _ _ = err "reconstruct" "unknown pattern"

    ungarble :: Wirelabel -> Bool
    ungarble wl = case M.lookup wl (ctx_truth ctx) of
      Nothing -> err "ungarble" $ "unknown wirelabel: " ++ showWirelabel wl
      Just b  -> b

    check :: Wirelabel -> Wirelabel
    check wl = case M.lookup wl (ctx_truth ctx) of
      Just _  -> wl
      Nothing -> err "check" ("no such wirelabel: " ++ showWirelabel wl)

traverseGG :: (Ref GarbledGate -> GarbledGate -> [Wirelabel] -> State Int Wirelabel) 
           -> Program GarbledGate 
           -> Int 
           -> State (Map (Ref GarbledGate) Wirelabel) ()
traverseGG construct prog z = foldM_ eval z (M.keys (env_deref (prog_env prog)))
  where
    getVal ref = get >>= \precomputed ->
      case M.lookup ref precomputed of
        Nothing  -> err "traverse.getVal" ("unknown ref: " ++ show ref)
        Just res -> return res

    eval s ref = do
      let c = lookupC ref prog
      kids <- mapM getVal (children c)
      let (result, s') = runState (construct ref c kids) s
      modify (M.insert ref result)
#ifdef DEBUG
      traceM ("[traverse] " ++ show ref ++ show (unRef <$> children c) 
             ++ " result = " ++ showWirelabel result)
#endif
      return s'

--------------------------------------------------------------------------------
-- helpers

nextIndex :: State Int Int
nextIndex = do
    c <- get
    put (succ c)
    return c
