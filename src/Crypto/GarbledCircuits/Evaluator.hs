module Crypto.GarbledCircuits.Evaluator
  (
    evalLocal
  , eval
  )
where

import Crypto.GarbledCircuits.Encryption
import Crypto.GarbledCircuits.Types
import Crypto.GarbledCircuits.Util

import           Control.Arrow (first, second)
import           Control.Monad.State
import           Control.Monad.Reader
import           Crypto.Cipher.AES
import qualified Data.Map as M
import qualified Data.Set as S

#ifdef DEBUG
import Debug.Trace
#endif

--------------------------------------------------------------------------------
-- garbled evaluator

type ResultMap = Map (Ref GarbledGate) Wirelabel
type Eval = ReaderT AES (State (Int, ResultMap))

runEval :: AES -> ResultMap -> Eval a -> ResultMap
runEval k m ev = snd $ execState (runReaderT ev k) (0,m)

eval :: Program GarbledGate -> AES -> [Wirelabel] -> [Wirelabel]
eval prog key inps =
#ifdef DEBUG
    trace (showGG prog) result
#else
    result
#endif
  where
    initialResults = M.fromList $ zip (S.toList (prog_inputs prog)) inps
    resultMap = runEval key initialResults (eval' prog)
    result    = map (resultMap !) (prog_outputs prog)

eval' :: Program GarbledGate -> Eval ()
eval' prog = mapM_ evalRef refs
  where
    refs = filter (`S.notMember` prog_inputs prog) (M.keys (env_deref (prog_env prog)))
    evalRef ref = do
      let c = lookupC ref prog
      kids   <- mapM getResult (children c)
      result <- construct c kids
      insertResult ref result
#ifdef DEBUG
      traceM ("[eval] " ++ show ref ++ show (unRef `fmap` children c)
           ++ " result = " ++ showWirelabel result)
#endif

construct :: GarbledGate -> [Wirelabel] -> Eval Wirelabel
construct (FreeXor  _ _    ) [a,b] =
    return (a `xor` b)
construct (HalfGate _ _ g e) [a,b] = do
    j1 <- nextIndex
    j2 <- nextIndex
    k  <- ask
    let wg  = hash k a j1 `xor` (lsb a `mask` g)
        we  = hash k b j2 `xor` (lsb b `mask` (e `xor` a))
    return (wg `xor` we)
construct _ _ = err "construct" "unknown pattern"

nextIndex :: Eval Int
nextIndex = do
    c <- gets fst
    modify (first succ)
    return c

getResult :: Ref GarbledGate -> Eval Wirelabel
getResult ref = gets snd >>= \precomputed ->
  case M.lookup ref precomputed of
    Nothing  -> err "getResult" ("unknown ref: " ++ show ref)
    Just res -> return res

insertResult :: Ref GarbledGate -> Wirelabel -> Eval ()
insertResult ref result = modify $ second (M.insert ref result)

-- evaluate a garbled circuit locally
evalLocal :: [Bool] -> (Program GarbledGate, Context) -> [Bool]
evalLocal inps (prog, ctx) =
#ifdef DEBUG
    trace (showPairs ctx) $
#endif
    map ungarble result
  where
    result   = eval prog (ctx_key ctx) inpwires
    inpwlps  = map (violentLookup $ ctx_pairs ctx) (S.toList $ prog_inputs prog)
    inpwires = zipWith sel inps inpwlps

    ungarble :: Wirelabel -> Bool
    ungarble wl = case M.lookup wl (ctx_truth ctx) of
      Nothing -> err "ungarble" $ "unknown wirelabel: " ++ showWirelabel wl
      Just b  -> b
