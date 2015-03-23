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
import Crypto.ObliviousTransfer

import           Control.Monad
import           Crypto.Cipher.AES128 (AESKey128)
import           Data.Functor
import qualified Data.ByteString.Char8 as BS
import           Data.Serialize (decode, encode, Serialize)
import           Network.Socket hiding (send, recv)
import           Network.BSD
import           System.IO

#ifdef DEBUG
import Debug.Trace
#else
traceM :: Monad m => String -> m ()
traceM _ = return ()
#endif

type Port = Int

data Connection = Connection { conn_send :: ByteString -> IO ()
                             , conn_recv :: Int -> IO ByteString
                             }

simpleConn :: Handle -> Connection
simpleConn h = Connection { conn_send = BS.hPut h, conn_recv = BS.hGet h }

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
      otSend conn theirPairs
      traceM "[garblerProto] recieving output"
      wires <- recv conn
      let result = map (ungarble ctx) wires
      traceM "[garblerProto] sending ungarbled output"
      send conn result
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
      inpEv <- otRecv conn inp
      traceM "[evaluatorProto] evaluating garbled circuit"
      let gg  = reconstruct tt hgs
          out = eval gg key inpGb inpEv
      traceM ("[evaluatorProto] output =\n" ++ showOutput (prog_output gg) out)
      traceM "[evaluatorProto] sending output wires"
      send conn out
      traceM "[evaluatorProto] recieving ungarbled output"
      recv conn

--------------------------------------------------------------------------------
-- ot

otSend :: Connection -> [(ByteString, ByteString)] -> IO ()
otSend conn elements = undefined

otRecv :: Connection -> [Bool] -> IO [ByteString]
otRecv onn choices = undefined

--------------------------------------------------------------------------------
-- network

send :: Serialize a => Connection -> a -> IO ()
send c x = do
    let encoding = encode x; n = BS.length encoding
    traceM ("[send] sending " ++ show n ++ " bytes")
    conn_send c (encode n)
    conn_send c encoding

recv :: Serialize a => Connection -> IO a
recv c = do
    num <- conn_recv c 8
    let n = either (err "recieve") id (decode num)
    str <- conn_recv c n
    traceM ("[recv] recieved " ++ show n ++ " bytes")
    either (err "recv") return (decode str)

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

showOutput :: [Ref GarbledGate] -> [Wirelabel] -> String
showOutput refs = init . unlines . zipWith (\r w -> "\t" ++ show r ++ " " ++ showWirelabel w) refs

asTuple :: WirelabelPair -> (Wirelabel, Wirelabel)
asTuple p = (wlp_false p, wlp_true p)
