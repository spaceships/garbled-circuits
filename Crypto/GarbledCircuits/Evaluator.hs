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

-- evaluate a garbled circuit locally
evalLocal :: [Bool] -> (Program GarbledGate, Context) -> [Bool]
evalLocal inps (prog, ctx) = map ungarble result
  where
    result   = evalProg reconstruct prog inpwires
    inpwlps  = map (violentLookup $ ctx_pairs ctx) (Set.toList $ prog_inputs prog)
    inpwires = zipWith sel inps inpwlps
    inputs   = zip (map InputId [0..]) inpwires

    reconstruct :: Ref GarbledGate -> GarbledGate -> [Wirelabel] -> Wirelabel
    reconstruct _ (GarbledInput id) [] = case lookup id inputs of
      Nothing -> err "reconstruct" ("no input wire with id " ++ show id ++ "\n" ++ show inputs)
      Just wl -> wl
    reconstruct ref g [x,y] = case lookup (wl_col x, wl_col y) (gate_table g) of
      Nothing -> err "reconstruct" "no matching color"
      Just z  -> let new_val = dec (ctx_key ctx) ref x y (wl_val z)
                     new_wl  = z { wl_val = new_val }
                 in new_wl
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

