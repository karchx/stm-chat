{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel, waitAnyCancel)
import Control.Concurrent.STM
import Control.Monad (forever, void, when)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.IO

type ClientId = Int
type RoomName = Text
data Client = Client
    { clientId :: ClientId
    , clientName :: Text
    , clientChan :: TChan Text -- messages to client
    }

data Server = Server
    { nextClientId :: TVar ClientId
    , clients :: TVar (Map ClientId Client)
    , rooms :: TVar (Map RoomName (TVar [ClientId]))
    }

newServer :: IO Server
newServer = atomically $ do
    nc <- newTVar 0
    cs <- newTVar Map.empty
    rs <- newTVar Map.empty
    return $ Server nc cs rs

registerClient :: Server -> Text -> IO Client
registerClient srv name = atomically $ do
    cid <- readTVar (nextClientId srv)
    writeTVar (nextClientId srv) (cid + 1)
    chan <- newTChan
    let client = Client cid name chan
    modifyTVar' (clients srv) (Map.insert cid client)
    return client

unregisterClient :: Server -> ClientId -> IO ()
unregisterClient srv cid = atomically $ do
    modifyTVar' (clients srv) (Map.delete cid)
    rmap <- readTVar (rooms srv)
    let removeCid tv = do
          lst <- readTVar tv
          writeTVar tv (filter (/= cid) lst)
    mapM_ removeCid (Map.elems rmap)

joinRoom :: Server -> RoomName -> ClientId -> IO ()
joinRoom srv room cid = atomically $ do
    rmap <- readTVar (rooms srv)
    tv <- case Map.lookup room rmap of
        Just t -> return t
        Nothing -> do
            t <- newTVar []
            writeTVar (rooms srv) (Map.insert room t rmap)
            return t
    modifyTVar' tv (\lst -> if cid `elem` lst then lst else cid:lst)

leaveRoom :: Server -> RoomName -> ClientId -> IO ()
leaveRoom srv room cid = atomically $ do
    rmap <- readTVar (rooms srv)
    case Map.lookup room rmap of
        Just tv -> modifyTVar' tv (filter (/= cid))
        Nothing -> return ()

broadcastToRoom :: Server -> RoomName -> Text -> IO ()
broadcastToRoom srv room msg = atomically $ do
    rmap <- readTVar (rooms srv)
    cmap <- readTVar (clients srv)
    case Map.lookup room rmap of
        Nothing -> return ()
        Just tv -> do
            cids <- readTVar tv
            let recipients = [ c | cid <- cids, Just c <- [Map.lookup cid cmap] ]
            mapM_ (\c -> writeTChan (clientChan c) msg) recipients

sendDirect :: Server -> ClientId -> Text -> IO ()
sendDirect srv cid msg = atomically $ do
    cmap <- readTVar (clients srv)
    case Map.lookup cid cmap of
        Just c -> writeTChan (clientChan c) msg
        Nothing -> return ()

clientLoop :: Server -> Client -> IO ()
clientLoop srv client = do
  let cid = clientId client
      cname = clientName client
      ch = clientChan client
  putStrLn $ "Client " ++ show cid ++ " (" ++ T.unpack cname ++ ") started."
  reader <- async $ forever $ do
    msg <- atomically $ readTChan ch
    putStrLn $ "[" ++ show cid ++ "] msg: " ++ T.unpack msg
  -- simulate sending messages
  -- For demo, every 2s broadcast to "main"
  let loop n = do
        broadcastToRoom srv "main" (T.concat [cname, ": hello #", T.pack (show n)])
        threadDelay (2 * 1000 * 1000)
        loop (n+1)
  sender <- async $ loop (1::Int)
  waitAnyCancel [reader, sender]
  putStrLn $ "Client loop ended: " ++ show cid
  cancel reader
  cancel sender


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  srv <- newServer
  clients <- mapM (\i -> registerClient srv (T.pack $ "user" ++ show i)) [1..10]
  mapM_ (\c -> joinRoom srv "main" (clientId c)) clients
  as <- mapM (async . clientLoop srv) clients
  threadDelay (20 * 1000 * 1000)
  putStrLn "Shutting down..."
  mapM_ (\c -> unregisterClient srv (clientId c)) clients
  mapM_ cancel as
  putStrLn "Done."
