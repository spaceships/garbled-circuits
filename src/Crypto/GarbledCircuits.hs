{-# LANGUAGE FlexibleInstances, RankNTypes #-}

module Crypto.GarbledCircuits (
    -- * Garbled Circuit datatypes
    Circuit (..)
  , Program (..)
  , Party (..)
  , Ref (..)
  -- * The garbled circuit protocols
  , Connection (..)
  , garblerProto
  , evaluatorProto
  -- ** Simple socket connection
  , connectTo
  , listenAt
  , simpleConn
  )
where

import Crypto.GarbledCircuits.GarbledGate
import Crypto.GarbledCircuits.TruthTable
import Crypto.GarbledCircuits.Eval
import Crypto.GarbledCircuits.Types
import Crypto.GarbledCircuits.Util
import Crypto.GarbledCircuits.Network
import Crypto.GarbledCircuits.ObliviousTransfer

import           Control.Monad
import           Crypto.Cipher.AES128 (AESKey128)
import           Data.Functor
import qualified Data.ByteString.Char8 as BS
import           Data.Serialize (decode, encode, Serialize)
import           Network.Socket hiding (send, recv)
import           Network.BSD
import           System.IO

garblerProto :: Program Circuit -> [Bool] -> Connection -> IO [Bool]
garblerProto prog inp conn = do
    (gg, ctx) <- garble prog
    traceM "[garblerProto] circuit garbled"
    let myWires    = inputWires Garbler   gg ctx inp
        theirPairs = map asTuple $ inputPairs Evaluator gg ctx
    traceM "[garblerProto] sending circuit"
    send conn (halfGates gg)
    traceM "[garblerProto] sending my input wires"
    send conn myWires
    traceM "[garblerProto] sending key"
    send conn (ctx_key ctx)
    traceM "[garblerProto] performing OT"
    otSend conn (ctx_key ctx) theirPairs
    traceM "[garblerProto] recieving output"
    wires <- recv conn
    let result = map (ungarble ctx) wires
    traceM "[garblerProto] sending ungarbled output"
    send conn result
    printConnectionInfo conn
    return result

evaluatorProto :: Program Circuit -> [Bool] -> Connection -> IO [Bool]
evaluatorProto prog inp conn = do
    let tt = circ2tt prog
    traceM "[evaluatorProto] recieving circuit"
    hgs <- recv conn :: IO [(Wirelabel,Wirelabel)]
    traceM "[evaluatorProto] recieving garbler input wires"
    inpGb <- recv conn :: IO [Wirelabel]
    traceM "[evaluatorProto] recieving key"
    key <- recv conn :: IO AESKey128
    traceM "[evaluatorProto] performing OT"
    inpEv <- otRecv conn key inp
    traceM "[evaluatorProto] evaluating garbled circuit"
    let gg  = reconstruct tt hgs
        out = eval gg key inpGb inpEv
    traceM ("[evaluatorProto] output =\n" ++ showOutput (prog_output gg) out)
    traceM "[evaluatorProto] sending output wires"
    send conn out
    traceM "[evaluatorProto] recieving ungarbled output"
    result <- recv conn
    printConnectionInfo conn
    return result

--------------------------------------------------------------------------------
-- ot

showOutput :: [Ref GarbledGate] -> [Wirelabel] -> String
showOutput refs = init . unlines . zipWith (\r w -> "\t" ++ show r ++ " " ++ showWirelabel w) refs

asTuple :: WirelabelPair -> (Wirelabel, Wirelabel)
asTuple p = (wlp_false p, wlp_true p)
