{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module BudgetChat (runBudgetChat) where

import Control.Concurrent.Async (race)
import Control.Concurrent.STM
import Control.Exception (finally, mask)
import Control.Monad (forever, void, when)
import Data.Char (isAlphaNum)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Simple.TCP

type ClientName = Text

data Client = Client
  { clientName :: ClientName
  , clientSocket :: Socket
  , clientSendChan :: TChan Message
  , clientBroadcastChan :: TChan Message
  }

data Server = Server
  { clients :: TVar (Map ClientName Client)
  , broadcastChan :: TChan Message
  }

data Message
  = Notice Text -- System messages (starts with *)
  | Broadcast ClientName Text -- Broadcasted message
  | Command Text -- Text inputted by user

isValidName :: Text -> Bool
isValidName name =
  not (T.null name)
    && T.length name <= 16
    && T.all isAlphaNum name

newClient :: ClientName -> Socket -> Server -> STM Client
newClient name socket Server{..} =
  Client name socket <$> newTChan <*> dupTChan broadcastChan

newServer :: IO Server
newServer = Server <$> newTVarIO M.empty <*> newBroadcastTChanIO

broadcast :: Server -> Message -> STM ()
broadcast Server{..} = writeTChan broadcastChan

getUserList :: Server -> ClientName -> IO Text
getUserList Server{..} excludeName = do
  clientMap <- readTVarIO clients
  let names = filter (/= excludeName) $ M.keys clientMap
  return $ "* The room contains: " <> T.intercalate ", " names

leaveClient :: Server -> ClientName -> STM ()
leaveClient server@Server{..} name = do
  modifyTVar' clients $ M.delete name
  broadcast server $ Notice $ name <> " has left the room"

sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} = writeTChan clientSendChan

checkAddClient :: Server -> ClientName -> Socket -> IO (Either Text Client)
checkAddClient server@Server{..} name socket
  | not (isValidName name) =
      return $ Left "Invalid name: must be <= 16 characters and all alphanumeric"
  | otherwise = atomically $ do
      clientMap <- readTVar clients
      if M.member name clientMap
        then return $ Left "Name is already in use"
        else do
          broadcast server $ Notice $ name <> " has entered the room"
          client <- newClient name socket server
          writeTVar clients $ M.insert name client clientMap
          return $ Right client

runBudgetChat :: ServiceName -> IO ()
runBudgetChat port = do
  server <- newServer
  serve (Host "0.0.0.0") port $ \(socket, _) -> talk socket server

talk :: Socket -> Server -> IO ()
talk socket server = do
  send socket "Welcome to budgetchat! What shall I call you?\n"
  readName
 where
  readName = do
    maybeNameBytes <- recv socket 1024
    case maybeNameBytes of
      Nothing -> return () -- client disconnected
      Just nameBytes -> case T.strip <$> TE.decodeUtf8' nameBytes of
        Left _ -> do
          send socket $ TE.encodeUtf8 "Error: Invalid UTF-8\n"
          readName
        Right name ->
          if T.null name
            then readName -- empty line
            else mask $ \restore -> do
              ok <- checkAddClient server name socket
              case ok of
                Left e -> send socket $ TE.encodeUtf8 $ e <> "\n"
                Right client -> do
                  restore (runClient server client)
                    `finally` atomically (leaveClient server name)

runClient :: Server -> Client -> IO ()
runClient serv client@Client{..} = do
  userList <- getUserList serv clientName
  send clientSocket $ TE.encodeUtf8 $ userList <> "\n"
  void $ race broadcaster $ race server receiver
 where
  receiver = do
    maybeBytes <- recv clientSocket 4096
    case maybeBytes of
      Nothing -> return ()
      Just bytes -> case filter (not . T.null) . map T.strip . T.lines <$> TE.decodeUtf8' bytes of
        Left _ -> do
          send clientSocket $ TE.encodeUtf8 "Error: Invalid UTF-8\n"
          receiver
        Right messages -> do
          mapM_ (atomically . sendMessage client . Command) messages
          receiver

  server = forever $ do
    msg <- atomically $ readTChan clientSendChan
    handleMessage serv client msg

  broadcaster = forever $ do
    msg <- atomically $ readTChan clientBroadcastChan
    return $ void $ handleMessage serv client msg

handleMessage :: Server -> Client -> Message -> IO ()
handleMessage server Client{..} message = case message of
  Notice msg -> send clientSocket $ TE.encodeUtf8 $ "* " <> msg <> "\n"
  Broadcast name msg ->
    when (name /= clientName) $
      send clientSocket $
        TE.encodeUtf8 $
          "[" <> name <> "]: " <> msg <> "\n"
  Command msg -> atomically $ broadcast server $ Broadcast clientName msg
