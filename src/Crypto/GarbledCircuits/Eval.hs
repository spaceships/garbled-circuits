module Crypto.GarbledCircuits.Eval
  ( evalLocal
  , eval
  )
where

import Crypto.GarbledCircuits.Encryption
import Crypto.GarbledCircuits.Types
import Crypto.GarbledCircuits.Util

import           Control.Arrow (first, second)
import           Control.Monad.State
import           Control.Monad.Reader
import           Crypto.Cipher.AES128
import           Data.Functor
import           Data.List (elemIndex)
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S

--------------------------------------------------------------------------------
-- garbled evaluator

type ResultMap = Map (Ref GarbledGate) Wirelabel
type Eval = ReaderT AESKey128 (State (Int, ResultMap))

runEval :: AESKey128 -> ResultMap -> Eval a -> ResultMap
runEval k m ev = snd $ execState (runReaderT ev k) (0,m)

eval :: Program GarbledGate -> AESKey128 -> [Wirelabel] -> [Wirelabel] -> [Wirelabel]
eval prog key inpGb inpEv = trace (showGG prog inpGb inpEv) result
  where
    initialResults = M.fromList (zip (S.toList (prog_input_gb prog)) inpGb) `M.union`
                     M.fromList (zip (S.toList (prog_input_ev prog)) inpEv)
    resultMap = runEval key initialResults (eval' prog)
    result    = map (resultMap !!!) (prog_output prog)

eval' :: Program GarbledGate -> Eval ()
eval' prog = mapM_ evalRef (nonInputRefs prog)
  where
    evalRef ref = do
      let c = lookupC ref prog
      kids   <- mapM getResult (children c)
      result <- construct c kids
      insertResult ref result
      traceM ("[eval] " ++ show ref ++ show (unRef `fmap` children c)
          ++ " " ++ typeOf c ++ " result = " ++ showWirelabel result)

construct :: GarbledGate -> [Wirelabel] -> Eval Wirelabel
construct (FreeXor  _ _    ) [a,b] =
    return (a `xorBytes` b)
construct (HalfGate _ _ g e) [a,b] = do
    j1 <- nextIndex
    j2 <- nextIndex
    k  <- ask
    let wg  = hash k a j1 `xorBytes` (lsb a `mask` g)
        we  = hash k b j2 `xorBytes` (lsb b `mask` (e `xorBytes` a))
    return (wg `xorBytes` we)
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

nonInputRefs :: Program c -> [Ref c]
nonInputRefs prog = filter (not.isInput) (M.keys (prog_env prog))
  where
    isInput ref = S.member ref (S.union (prog_input_gb prog) (prog_input_ev prog))

-- evaluate a garbled circuit locally
evalLocal :: [Bool] -> [Bool] -> (Program GarbledGate, Context) -> [Bool]
evalLocal inpGb inpEv (prog, ctx) =
    trace (showPairs ctx ++ "[evalLocal] result = " ++ show result) result
  where
    result = map (ungarble ctx) outs
    outs   = eval prog (ctx_key ctx) aWires bWires
    aWires = inputWires Garbler   prog ctx inpGb
    bWires = inputWires Evaluator prog ctx inpEv

typeOf :: GarbledGate -> String
typeOf (GarbledInput _ _) = "Input"
typeOf (FreeXor _ _)      = "FreeXor"
typeOf (HalfGate {})      = "HalfGate"

showGG :: Program GarbledGate -> [Wirelabel] -> [Wirelabel] -> String
showGG prog inpGb inpEv = init $ unlines $ map showGate (M.toList (prog_env prog))
  where
    showGate (ref, gg) = show ref ++ ": " ++ case gg of
        GarbledInput i p -> show i ++ " " ++ show p ++ " " ++ outp ref ++ partyInput p i
        FreeXor  x y     -> "FREEXOR "  ++ show x ++ " " ++ show y ++ " " ++ outp ref
        HalfGate x y g e -> "HALFGATE " ++ show x ++ " " ++ show y ++ " " ++ outp ref ++ "\n"
                                      ++ "\t" ++ showWirelabel g ++ "\n"
                                      ++ "\t" ++ showWirelabel e
    outp r = case r `elemIndex` prog_output prog
      of Just i -> "out" ++ show i; _ -> ""

    partyInput Garbler   (InputId i) | length inpGb > i = showWirelabel (inpGb !! i)
    partyInput Evaluator (InputId i) | length inpEv > i = showWirelabel (inpEv !! i)
    partyInput _ _ = ""

showPairs :: Context -> String
showPairs ctx =
    "--------------------------------------------------------------------------------\n"
    ++ "-- pairs \n" ++ concatMap showPair (M.toList (ctx_pairs ctx))
  where
    showPair (ref, pair) = show ref ++ ": true=" ++ showWirelabel (wlp_true pair)
                                    ++ " false=" ++ showWirelabel (wlp_false pair) ++ "\n"

