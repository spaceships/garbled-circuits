{-# LANGUAGE FlexibleInstances #-}

module Crypto.GarbledCircuits
  ( garblerProto
  , evaluatorProto
  , Builder
  , buildCircuit
  , evalCircuit
  )
where

import Crypto.GarbledCircuits.Language
import Crypto.GarbledCircuits.GarbledGate
import Crypto.GarbledCircuits.TruthTable
import Crypto.GarbledCircuits.Eval
import Crypto.GarbledCircuits.Types
import Crypto.GarbledCircuits.Util

import           Crypto.Cipher.AES
import           Control.Monad
import           Data.Functor
import qualified Data.ByteString.Char8 as BS
import           Data.Serialize
import           Data.Word
import           Network.Socket hiding (send)
import           Network.BSD
import           System.IO

import Debug.Trace

type Port = Int

garblerProto :: Port -> Program Circuit -> [Bool] -> IO [Bool]
garblerProto port prog inp = do
    (gg, ctx) <- garble prog
    traceM "[garblerProto] circuit garbled"
    let myWires    = inputWires PartyA gg ctx inp
        theirPairs = inputPairs PartyB gg ctx
    listenAt port $ \h -> do
      traceM "[garblerProto] sending circuit"
      send h (halfGates gg)
      traceM "[garblerProto] sending my input wires"
      send h myWires
      traceM "[garblerProto] sending key"
      send h (fst (ctx_key ctx))
      traceM "[garblerProto] performing OT"
      otSendWirelabels h theirPairs
      traceM "[garblerProto] recieving output"
      wires <- recieve h
      let result = map (ungarble ctx) wires
      traceM "[garblerProto] sending ungarbled output"
      send h result
      return result

evaluatorProto :: HostName -> Port -> Program Circuit -> [Bool] -> IO [Bool]
evaluatorProto host port prog inp = do
    let tt = circ2tt prog
    connectTo host port $ \h -> do
      traceM "[evaluatorProto] recieving circuit"
      hgs <- recieve h :: IO [(Wirelabel,Wirelabel)]
      traceM "[evaluatorProto] recieving garbler input wires"
      inpA <- recieve h :: IO [Wirelabel]
      traceM "[evaluatorProto] recieving key"
      key <- recieve h :: IO ByteString
      traceM "[evaluatorProto] performing OT"
      inpB <- otRecvWirelabels h inp
      traceM "[evaluatorProto] evaluating garbled circuit"
      let gg  = reconstruct tt hgs
          k   = initAES (key :: ByteString)
          out = eval gg k inpA inpB
      traceM ("[evaluatorProto] output =\n" ++ showOutput (prog_output gg) out)
      traceM "[evaluatorProto] sending output wires"
      send h out
      traceM "[evaluatorProto] recieving ungarbled output"
      recieve h

showOutput :: [Ref GarbledGate] -> [Wirelabel] -> String
showOutput refs = init . unlines . zipWith (\r w -> "\t" ++ show r ++ " " ++ showWirelabel w) refs

send :: Serialize a => Handle -> a -> IO ()
send h x = do
    let encoding = encode x
        n        = BS.length encoding
    traceM ("[send] sending " ++ show n ++ " bytes")
    BS.hPut h (encode n)
    BS.hPut h encoding

recieve :: Serialize a => Handle -> IO a
recieve h = do
    num <- BS.hGet h 8
    let n = either (err "recieve") id (decode num)
    str <- BS.hGet h n
    traceM ("[recieve] recieved " ++ show n ++ " bytes")
    either (err "recieve") return (decode str)

otSendWirelabels :: Handle -> [WirelabelPair] -> IO ()
otSendWirelabels h wlps = do
    traceM "[otSendWirelabels] WARNING: not actually OT"
    inps <- recieve h
    let wires = zipWith sel inps wlps
    send h wires

otRecvWirelabels :: Handle -> [Bool] -> IO [Wirelabel]
otRecvWirelabels h inps = do
    traceM "[otRecvWirelabels] WARNING: not actually OT"
    send h inps
    recieve h

connectTo :: HostName -> Port -> (Handle -> IO a) -> IO a
connectTo host port_ f = withSocketsDo $ do
    let port = toEnum port_
    sock <- socket AF_INET Stream 0
    addrs <- liftM hostAddresses $ getHostByName host
    when (null addrs) $ err "connectTo" ("no such host: " ++ host)
    connect sock $ SockAddrInet port (head addrs)
    perform sock f

listenAt :: Port -> (Handle -> IO a) -> IO a
listenAt port_ f = withSocketsDo $ do
    let port = toEnum port_
    lsock <- socket AF_INET Stream 0
    bindSocket lsock (SockAddrInet port iNADDR_ANY)
    listen lsock sOMAXCONN
    (sock,SockAddrInet _ _) <- accept lsock
    perform sock f

perform :: Socket -> (Handle -> IO a) -> IO a
perform sock f = withSocketsDo $ do
    handle <- socketToHandle sock ReadWriteMode
    result <- f handle
    hClose handle
    return result
