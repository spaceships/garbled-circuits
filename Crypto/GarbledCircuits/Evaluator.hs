module Crypto.GarbledCircuits.Evaluator
  (
    evalLocal
  )
where

import Crypto.GarbledCircuits.Encryption
import Crypto.GarbledCircuits.Types
import Crypto.GarbledCircuits.Util

import           Control.Monad
import qualified Data.Map         as M
import qualified Data.Set         as Set
import           Debug.Trace

--------------------------------------------------------------------------------
-- garbled evaluator

{-eval :: [Wirelabel] -> Program GarbledGate -> [Wirelabel]-}
{-eval inps prog =-}

-- evaluate a garbled circuit locally
evalLocal :: [Bool] -> (Program GarbledGate, Context) -> IO [Bool]
evalLocal inps (prog, ctx) = do
    result <- evalProg reconstruct prog inpwires :: IO [Wirelabel]
#ifdef DEBUG
    let out_pairs  = map (\ref -> (ref, violentLookup (ctx_pairs ctx) ref)) (prog_outputs prog)
        out_truths = map (\(ref, pair) -> (ref, wlp_true pair, wlp_false pair)) out_pairs
    forM out_truths $ \(ref, t, f) -> do
      putStrLn ("[evalProg] outwire " ++ show ref ++ " true:  " ++ show t)
      putStrLn ("[evalProg] outwire " ++ show ref ++ " false: " ++ show f)
#endif
    return (map ungarble result)
  where
    k = ctx_key ctx

    inpwlps  = map (violentLookup $ ctx_pairs ctx) (Set.toList $ prog_inputs prog)
    inpwires = zipWith sel inps inpwlps
    inputs   = zip (map InputId [0..]) inpwires

    reconstruct :: Ref GarbledGate -> GarbledGate -> [Wirelabel] -> IO Wirelabel
    reconstruct _ (GarbledInput id) [] = case lookup id inputs of
      Nothing -> err "reconstruct" ("no input wire with id " ++ show id ++ "\n" ++ show inputs)
      Just wl -> return wl
    reconstruct ref g [x,y] = case lookup (wl_col x, wl_col y) (gate_table g) of
      Nothing -> err "reconstruct" "no matching color"
      Just z  -> do
        let new_val = dec k ref x y (wl_val z)
            new_wl  = z { wl_val = new_val }
        return new_wl
    reconstruct _ _ _ = err "reconstruct" "unknown pattern"

    ungarble :: Wirelabel -> Bool
    ungarble wl = case M.lookup wl (ctx_truth ctx) of
      Nothing -> err "ungarble" $ "unknown wirelabel: " ++ show wl
#ifdef DEBUG
                                  ++ "\n" ++ showEnv prog ++ showPairs ctx
#endif
      Just b  -> b

--------------------------------------------------------------------------------
-- helpers

showEnv :: Program GarbledGate -> String
showEnv prog =
    "--------------------------------------------------------------------------------\n"
    ++ "-- env \n" ++ concatMap showGate (M.toList (env_deref (prog_env prog)))
  where
    showGate (ref, gg) = show ref ++ ": " ++ case gg of
        GarbledInput id -> show id ++ "\n"
        _ -> show (gate_inpx gg) ++ " " ++ show (gate_inpy gg) ++ "\n"
             ++ concatMap showTabElem (gate_table gg)
    showTabElem (col, wl) = "\t" ++ showColor col ++ " " ++ show wl ++ "\n"
    showColor (b0, b1) = (if b0 then "1" else "0") ++ if b1 then "1" else "0"

showPairs :: Context -> String
showPairs ctx =
    "--------------------------------------------------------------------------------\n"
    ++ "-- pairs \n" ++ concatMap showPair (M.toList (ctx_pairs ctx))
  where
    showPair (ref, pair) = show ref ++ ": true=" ++ show (wlp_true pair)
                                    ++ " false=" ++ show (wlp_false pair) ++ "\n"

