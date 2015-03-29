module Crypto.GarbledCircuits.Network
  ( simpleConn
  , send
  , recv
  , send2
  , recv2
  , send4
  , recv4
  , connectTo
  , listenAt
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

--------------------------------------------------------------------------------
-- network

simpleConn :: Handle -> Connection
simpleConn h = Connection { conn_send = BS.hPut h, conn_recv = BS.hGet h }

send :: Serialize a => Connection -> a -> IO ()
send c x = do
    n <- send' c x
    {-traceM ("[send] sent " ++ show n ++ " bytes")-}
    return ()

send' :: Serialize a => Connection -> a -> IO Int
send' c x = do
    let encoding = encode x; n = BS.length encoding
    conn_send c (encode n)
    conn_send c encoding
    return (n + 8)

recv :: Serialize a => Connection -> IO a
recv c = do
    (x, n) <- recv' c
    {-traceM ("[recv] recieved " ++ show n ++ " bytes")-}
    return x

recv' :: Serialize a => Connection -> IO (a, Int)
recv' c = do
    num <- conn_recv c 8
    let n = either (err "recieve") id (decode num)
    str <- conn_recv c n
    either (err "recv") (\x -> return (x, n+8)) (decode str)

send2 :: Serialize a => Connection -> (a, a) -> IO ()
send2 conn (x,y) = do
    n <- sum <$> mapM (send' conn) [x,y]
    {-traceM ("[send2] sent " ++ show n ++ " bytes")-}
    return ()

recv2 :: Serialize a => Connection -> IO (a, a)
recv2 conn = do
    res <- replicateM 2 (recv' conn)
    let [x,y] = map fst res
        n = sum (map snd res)
    {-traceM ("[recv2] recieved " ++ show n ++ " bytes")-}
    return (x,y)

send4 :: Serialize a => Connection -> (a, a, a, a) -> IO ()
send4 conn (w,x,y,z) = do
    n <- sum <$> mapM (send' conn) [w,x,y,z]
    {-traceM ("[send4] sent " ++ show n ++ " bytes")-}
    return ()

recv4 :: Serialize a => Connection -> IO (a, a, a, a)
recv4 conn = do
    res <- replicateM 4 (recv' conn)
    let [w,x,y,z] = map fst res
        n = sum (map snd res)
    {-traceM ("[recv4] recieved " ++ show n ++ " bytes")-}
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
