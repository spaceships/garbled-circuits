module Crypto.GarbledCircuits.Eval
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
import           Data.Functor
import qualified Data.Map as M
import           Data.Maybe
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

eval :: Program GarbledGate -> AES -> [Wirelabel] -> [Wirelabel] -> [Wirelabel]
eval prog key inpA inpB =
#ifdef DEBUG
    trace (showGG prog inpA inpB) result
#else
    result
#endif
  where
    initialResults = M.fromList (zip (S.toList (prog_input_a prog)) inpA) `M.union`
                     M.fromList (zip (S.toList (prog_input_b prog)) inpB)
    resultMap = runEval key initialResults (eval' prog)
    result    = map (resultMap !) (prog_output prog)

eval' :: Program GarbledGate -> Eval ()
eval' prog = mapM_ evalRef (nonInputRefs prog)
  where
    evalRef ref = do
      let c = lookupC ref prog
      kids   <- mapM getResult (children c)
      result <- construct c kids
      insertResult ref result
#ifdef DEBUG
      traceM ("[eval] " ++ show ref ++ show (unRef `fmap` children c)
          ++ " " ++ typeOf c ++ " result = " ++ showWirelabel result)
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
construct gate args = err "construct" ("unknown pattern: \n" ++ show gate ++ "\n" ++ show args)

nextIndex :: Eval Int
nextIndex = do
    c <- gets fst
    modify (first succ)
    return c

getResult :: Ref GarbledGate -> Eval Wirelabel
getResult ref = fromMaybe (err "getResult" "no ref") <$> (M.lookup ref <$> gets snd)

insertResult :: Ref GarbledGate -> Wirelabel -> Eval ()
insertResult ref result = modify $ second (M.insert ref result)

-- evaluate a garbled circuit locally
evalLocal :: [Bool] -> [Bool] -> (Program GarbledGate, Context) -> [Bool]
evalLocal inpA inpB (prog, ctx) =
#ifdef DEBUG
    trace (showPairs ctx) $
    trace ("[evalLocal] result = " ++ show result)
#endif
    result
  where
    result = map (ungarble ctx) outs
    outs   = eval prog (snd (ctx_key ctx)) aWires bWires
    aWires = inputWires PartyA prog ctx inpA
    bWires = inputWires PartyB prog ctx inpB

typeOf :: GarbledGate -> String
typeOf (GarbledInput _ _) = "Input"
typeOf (FreeXor _ _)      = "FreeXor"
typeOf (HalfGate _ _ _ _) = "HalfGate"
