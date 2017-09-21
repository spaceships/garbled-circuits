module Crypto.GarbledCircuits.Network
  ( byteSize
  , simpleConn
  , send
  , recv
  , send2
  , recv2
  , send4
  , recv4
  , connectTo
  , listenAt
  , printConnectionInfo
  )
where

import Crypto.GarbledCircuits.Types
import Crypto.GarbledCircuits.Util

import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.Functor
import           Data.Serialize (decode, encode, Serialize)
import           Network.Socket hiding (send, recv)
import           Network.BSD
import           System.IO
import           Data.IORef
import           Text.Printf

--------------------------------------------------------------------------------
-- network

byteSize :: Serialize a => a -> Int
byteSize = BS.length . encode

simpleConn :: Handle -> IO Connection
simpleConn h = do
    zero0 <- newIORef 0
    zero1 <- newIORef 0
    Connection (BS.hPut h) (BS.hGet h) <$> newIORef 0 <*> newIORef 0

send :: Serialize a => Connection -> a -> IO ()
send c x = do
    let encoding = encode x; n = BS.length encoding
    conn_send c (encode n)
    conn_send c encoding
    modifyIORef' (conn_bytes_sent c) (+ (n+8))

recv :: Serialize a => Connection -> IO a
recv c = do
    num <- conn_recv c 8
    let n = either (err "recieve") id (decode num)
    str <- conn_recv c n
    modifyIORef' (conn_bytes_received c) (+ (n+8))
    either (err "recv") return (decode str)

send2 :: Serialize a => Connection -> (a, a) -> IO ()
send2 conn (x,y) = mapM_ (send conn) [x,y]

recv2 :: Serialize a => Connection -> IO (a, a)
recv2 conn = do
    [x,y] <- replicateM 2 (recv conn)
    return (x,y)

send4 :: Serialize a => Connection -> (a, a, a, a) -> IO ()
send4 conn (w,x,y,z) = mapM_ (send conn) [w,x,y,z]

recv4 :: Serialize a => Connection -> IO (a, a, a, a)
recv4 conn = do
    [w,x,y,z] <- replicateM 4 (recv conn)
    return (w,x,y,z)

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
    bind lsock (SockAddrInet port iNADDR_ANY)
    listen lsock sOMAXCONN
    (sock,SockAddrInet _ _) <- accept lsock
    perform sock f

perform :: Socket -> (Handle -> IO a) -> IO a
perform sock f = withSocketsDo $ do
    handle <- socketToHandle sock ReadWriteMode
    result <- f handle
    hClose handle
    return result

printConnectionInfo :: Connection -> IO ()
printConnectionInfo c = do
    sent  <- readIORef (conn_bytes_sent c)
    recvd <- readIORef (conn_bytes_received c)
    printf "[garblerProto] %d bytes sent, %d bytes received.\n" sent recvd
