{-# LANGUAGE LambdaCase, ScopedTypeVariables, FlexibleContexts #-}

module Crypto.GarbledCircuits.Util
  ( bitOr
  , bindM2
  , bits2Word
  , convertRef
  , err
  , evalProg
  , inputPairs
  , inputSize
  , inputWires
  , inputp
  , internp
  , lookp
  , lookupC
  , mask
  , nextRef
  , nonInputRefs
  , outputPairs
  , progSize
  , sel
  , showGG
  , showPairs
  , topoLevels
  , topoSort
  , truthVals
  , ungarble
  , violentLookup
  , word2Bits
  , writep
  , xor
  , xorWords
  , (!)
  )
where

import Crypto.GarbledCircuits.Types

import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Bits hiding (xor)
import qualified Data.Bits
import qualified Data.ByteString  as BS
import           Data.Functor
import           Data.List (elemIndex)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import qualified Data.Set as S
import           Data.Word

#ifdef DEBUG
import Debug.Trace
#endif

--------------------------------------------------------------------------------
-- general helper functions

bindM2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bindM2 f a b = do x <- a; y <- b; f x y

-- returns a little-endian list of bits
word2Bits :: (FiniteBits b, Num b, Ord b, Bits b) => b -> [Bool]
word2Bits x = map (bitAnd x) (take (finiteBitSize x) pow2s)
  where
    bitAnd a b = a .&. b > 0

-- takes a little-endian list of bits
bits2Word :: (Bits a, Num a) => [Bool] -> a
bits2Word bs = sum $ zipWith select bs pow2s
  where
    select b x = if b then x else 0

pow2s :: (Num b, Bits b) => [b]
pow2s = [ shift 1 x | x <- [0..] ]

progSize :: Program c -> Int
progSize = M.size . env_deref . prog_env

xor :: ByteString -> ByteString -> ByteString
xor x y | BS.length x /= BS.length y = err "xor" ("unequal length inputs: " ++ show (BS.length x) 
                                                                     ++ " " ++ show (BS.length y))
        | otherwise = BS.pack $ BS.zipWith Data.Bits.xor x y

bitOr :: ByteString -> ByteString -> ByteString
bitOr x y | BS.length x /= BS.length y = err "xor" "unequal length inputs"
          | otherwise = BS.pack $ BS.zipWith (.|.) x y

mask :: Bool -> Wirelabel -> Wirelabel
mask b wl = if b then wl else zeroWirelabel

xorWords :: [Word8] -> [Word8] -> [Word8]
xorWords = zipWith Data.Bits.xor

inputSize :: Party -> Program c -> Int
inputSize p prog = S.size (prog_inputs p prog)

nonInputRefs :: Program c -> [Ref c]
nonInputRefs prog = filter (not.isInput) (M.keys (env_deref (prog_env prog)))
  where
    isInput ref = S.member ref (S.union (prog_inputs_a prog) (prog_inputs_b prog))

--------------------------------------------------------------------------------
-- garbled gate helpers

sel :: Bool -> WirelabelPair -> Wirelabel
sel b = if b then wlp_true else wlp_false

inputWires :: Party -> Program GarbledGate -> Context -> [Bool] -> [Wirelabel]
inputWires party prog ctx inp = zipWith sel inp (inputPairs party prog ctx)

inputPairs :: Party -> Program GarbledGate -> Context -> [WirelabelPair]
inputPairs p prog ctx = map (ctx_pairs ctx !) (S.toList (prog_inputs p prog))

outputPairs :: Program GarbledGate -> Context -> [WirelabelPair]
outputPairs prog ctx = map (ctx_pairs ctx !) (prog_outputs prog)

ungarble :: Context -> Wirelabel -> Bool
ungarble ctx wl = case M.lookup wl (ctx_truth ctx) of
  Nothing -> err "ungarble" $ "unknown wirelabel: " ++ showWirelabel wl
  Just b  -> b

showGG :: Program GarbledGate -> String
showGG prog =
    "--------------------------------------------------------------------------------\n"
    ++ "-- env \n" ++ concatMap showGate (M.toList (env_deref (prog_env prog)))
  where
    showGate (ref, gg) = show ref ++ ": " ++ case gg of
        GarbledInput i p -> show i ++ show p ++ " " ++ outp ref ++ "\n"
        FreeXor  x y     -> "FREEXOR "  ++ show x ++ " " ++ show y ++ " " ++ outp ref ++ "\n"
        HalfGate x y g e -> "HALFGATE " ++ show x ++ " " ++ show y ++ " " ++ outp ref ++ "\n"
                                      ++ "\t" ++ showWirelabel g ++ "\n"
                                      ++ "\t" ++ showWirelabel e ++ "\n"
    outp r = case r `elemIndex` prog_outputs prog
      of Just i -> "out" ++ show i; _ -> ""

showPairs :: Context -> String
showPairs ctx =
    "--------------------------------------------------------------------------------\n"
    ++ "-- pairs \n" ++ concatMap showPair (M.toList (ctx_pairs ctx))
  where
    showPair (ref, pair) = show ref ++ ": true=" ++ showWirelabel (wlp_true pair)
                                    ++ " false=" ++ showWirelabel (wlp_false pair) ++ "\n"

--------------------------------------------------------------------------------
-- polymorphic helper functions for State monads over a Program

nextRef :: (Ord c, MonadState (Program c) m) => m (Ref c)
nextRef = do
  prog <- get
  let env   = prog_env prog
      deref = env_deref env
  return $ succ (fst (M.findMax deref))

internp :: (Ord c, MonadState (Program c) m) => c -> m (Ref c)
internp circ = do
  prog <- get
  let env   = prog_env prog
      dedup = env_dedup env
      deref = env_deref env
  case M.lookup circ dedup of
    Just ref -> return ref
    Nothing  -> do
      let ref    = if M.null deref then Ref 0 else succ $ fst (M.findMax deref)
          dedup' = M.insert circ ref dedup
          deref' = M.insert ref circ deref
          env'   = env { env_dedup = dedup', env_deref = deref' }
      put prog { prog_env = env' }
      return ref

inputp :: (Ord c, MonadState (Program c) m) => Party -> c -> m (Ref c)
inputp party inp = do
  ref <- internp inp
  modify $ \p -> case party of 
    A -> p { prog_inputs_a = S.insert ref (prog_inputs_a p) }
    B -> p { prog_inputs_b = S.insert ref (prog_inputs_b p) }
  return ref

writep :: (Ord c, MonadState (Program c) m) => Ref c -> c -> m ()
writep ref circ = do
  prog <- get
  let env   = prog_env prog
      dedup = M.insert circ ref (env_dedup env)
      deref = M.insert ref circ (env_deref env)
      env'  = env { env_dedup = dedup, env_deref = deref }
  put prog { prog_env = env' }

lookp :: (Ord c, MonadState (Program c) m) => Ref c -> m c
lookp ref = do
  env <- gets prog_env
  case M.lookup ref (env_deref env) of
    Nothing -> error "[lookp] no c"
    Just c  -> return c

lookupC :: Ref c -> Program c -> c
lookupC ref prog = fromMaybe (error "[lookupC] no c") (M.lookup ref deref)
  where
    deref = env_deref (prog_env prog)

--------------------------------------------------------------------------------
-- polymorphic topoSort

data DFSSt c = DFSSt { dfs_todo :: Set (Ref c)
                     , dfs_done :: Set (Ref c)
                     }

type DFS c = WriterT [Ref c] (State (DFSSt c))

topoSort :: CanHaveChildren c => Program c -> [Ref c]
topoSort prog = evalState (execWriterT (loop prog)) initialState
  where
    deref = env_deref (prog_env prog)

    initialState = DFSSt { dfs_todo = S.fromList (M.keys deref)
                         , dfs_done = S.empty
                         }

    loop :: CanHaveChildren c => Program c -> DFS c ()
    loop prg = next >>= \case
      Just ref -> visit prg ref >> loop prg
      Nothing  -> return ()

    visit :: CanHaveChildren c => Program c -> Ref c -> DFS c ()
    visit prg ref = do
      done <- gets dfs_done
      when (S.notMember ref done) $ do
        let circ = lookupC ref prg
        mapM_ (visit prg) (children circ)
        mark ref

    next :: DFS c (Maybe (Ref c))
    next = do
      st <- get
      let todo = dfs_todo st
      if S.size todo > 0 then do
        let ref = S.findMin todo
        put st { dfs_todo = S.delete ref todo }
        return $ Just ref
      else
        return Nothing

    mark :: Ref c -> DFS c ()
    mark ref = do
      st <- get
      put st { dfs_done = S.insert ref (dfs_done st) }
      tell [ref]

topoLevels :: CanHaveChildren c => Program c -> [[Ref c]]
topoLevels prog = S.toList . fst <$> foldl foldTopo [] (topoSort prog)
  where
    update ref (set, deps) = 
      let kids = children (lookupC ref prog) 
      in if any (`S.member` deps) kids
         then Left  $ S.insert ref deps
         else Right (S.insert ref set, S.insert ref deps)

    foldTopo [] ref = [(S.singleton ref, S.singleton ref)]
    foldTopo ((s,d):sets) ref = case update ref (s,d) of
      Right (s',d') -> (s',d') : sets
      Left  d'      -> (s ,d') : foldTopo sets ref

--------------------------------------------------------------------------------
-- polymorphic evaluation

evalProg :: (Show b, CanHaveChildren c)
         => (Ref c -> c -> [b] -> b) -> Program c -> [b]
evalProg construct prog = outputs
  where
    resultMap = execState (traverse construct prog) M.empty
    outputs   = map (resultMap !) (prog_outputs prog)

traverse :: (Show b, MonadState (Map (Ref c) b) m, CanHaveChildren c)
         => (Ref c -> c -> [b] -> b) -> Program c -> m ()
traverse construct prog = mapM_ eval (M.keys (env_deref (prog_env prog)))
  where
    getVal ref = get >>= \precomputed ->
      case M.lookup ref precomputed of
        Nothing  -> err "traverse.getVal" ("unknown ref: " ++ show ref)
        Just res -> return res
    eval ref = do
      let c = lookupC ref prog
      kids <- mapM getVal (children c)
      let result = construct ref c kids
      modify (M.insert ref result)
#ifdef DEBUG
      traceM ("[traverse] " ++ show ref ++ show (unRef <$> children c) 
             ++ " result = " ++ show result)
#endif
      return result

--------------------------------------------------------------------------------
-- evil helpers

convertRef :: Ref a -> Ref b
convertRef = Ref . unRef

err :: String -> String -> a
err name warning = error $ "[" ++ name ++ "] " ++ warning

(!) :: (Show k, Show v, Ord k) => Map k v -> k -> v
(!) = violentLookup

violentLookup :: (Show k, Show v, Ord k) => Map k v -> k -> v
violentLookup m k = case M.lookup k m of
  Nothing -> err "violentLookup" ("OOPS: " ++ show m)
  Just  v -> v
