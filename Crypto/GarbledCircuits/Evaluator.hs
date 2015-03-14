module Crypto.GarbledCircuits.Evaluator
  (
    evalLocal
  )
where

import Crypto.GarbledCircuits.Encryption
import Crypto.GarbledCircuits.Types
import Crypto.GarbledCircuits.Util

import           Data.List (elemIndex)
import qualified Data.Map         as M
import qualified Data.Set         as Set
import           Debug.Trace

--------------------------------------------------------------------------------
-- garbled evaluator

-- evaluate a garbled circuit locally
evalLocal :: [Bool] -> (Program GarbledGate, Context) -> [Bool]
evalLocal inps (prog, ctx) =
#ifdef DEBUG
    trace (showEnv prog ++ showPairs ctx) $
#endif
    map ungarble result
  where
    result   = evalProg reconstruct prog
    inpwlps  = map (violentLookup $ ctx_pairs ctx) (Set.toList $ prog_inputs prog)
    inpwires = zipWith sel inps inpwlps
    inputs   = zip (map InputId [0..]) inpwires

    reconstruct :: Ref GarbledGate -> GarbledGate -> [Wirelabel] -> Wirelabel

    reconstruct _ (GarbledInput i) [] = case lookup i inputs of
      Nothing -> err "reconstruct" ("no input wire with id " ++ show i ++ "\n" ++ show inputs)
      Just wl -> wl

    reconstruct _ (GarbledXor _ _) [x,y] = xorWires x y

    reconstruct ref (GarbledGate _ _ tab) [x,y] = case lookup (wl_col x, wl_col y) tab of
      Nothing -> err "reconstruct" "no matching color"
      Just z  -> let new_val = dec (ctx_key ctx) ref x y (wl_val z)
                     new_wl  = z { wl_val = new_val }
                 in check new_wl

    reconstruct _ _ _ = err "reconstruct" "unknown pattern"

    ungarble :: Wirelabel -> Bool
    ungarble wl = case M.lookup wl (ctx_truth ctx) of
      Nothing -> err "ungarble" $ "unknown wirelabel: " ++ show wl
      Just b  -> b

    check :: Wirelabel -> Wirelabel
    check wl = case M.lookup wl (ctx_truth ctx) of
      Just _  -> wl
      Nothing -> err "check" ("no such wirelabel: " ++ show wl)

--------------------------------------------------------------------------------
-- helpers

showEnv :: Program GarbledGate -> String
showEnv prog =
    "--------------------------------------------------------------------------------\n"
    ++ "-- env \n" ++ concatMap showGate (M.toList (env_deref (prog_env prog)))
  where
    showGate (ref, gg) = show ref ++ ": " ++ case gg of
        GarbledInput i      -> show i ++ " " ++ outp ref ++ "\n"
        GarbledGate x y tab -> show x ++ " " ++ show y ++ " " ++ outp ref ++ "\n"
                                      ++ concatMap showTabElem tab
        GarbledXor  x y     -> "XOR " ++ show x ++ " " ++ show y ++ " " ++ outp ref ++ "\n"
    showTabElem (col, wl) = "\t" ++ showColor col ++ " " ++ show wl ++ "\n"
    showColor (b0, b1) = (if b0 then "1" else "0") ++ if b1 then "1" else "0"
    outp r = case r `elemIndex` prog_outputs prog
      of Just i -> "out" ++ show i; _ -> ""

showPairs :: Context -> String
showPairs ctx =
    "--------------------------------------------------------------------------------\n"
    ++ "-- pairs \n" ++ concatMap showPair (M.toList (ctx_pairs ctx))
  where
    showPair (ref, pair) = show ref ++ ": true=" ++ show (wlp_true pair)
                                    ++ " false=" ++ show (wlp_false pair) ++ "\n"
