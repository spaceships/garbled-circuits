module Crypto.GarbledCircuits
  (
    garblerProto
  , evaluatorProto
  , module Crypto.GarbledCircuits.Language
  )
where

import Crypto.GarbledCircuits.Language
import Crypto.GarbledCircuits.GarbledGate
import Crypto.GarbledCircuits.Eval
import Crypto.GarbledCircuits.Types
import Crypto.GarbledCircuits.Util

import           Control.Exception
import           Control.Monad
import qualified Data.ByteString as BS
import           Data.Serialize
import           Network.Socket
import           Network.BSD
import           System.IO

type Port = Int

garblerProto :: Port -> Program Circ -> [Bool] -> IO [Bool]
garblerProto port prog inp = do
    (gg, ctx) <- garble prog
    let myPairs    = inputPairs A gg ctx
    let theirPairs = inputPairs B gg ctx
    listenAt port $ \h -> do
      undefined
      {-BS.hPutStrLn h (encode gg)-}
    undefined

evaluatorProto :: HostName -> Port -> [Bool] -> IO [Bool]
evaluatorProto = undefined

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
