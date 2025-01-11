{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module SpeedDaemon.Client where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM
import Control.Exception (bracket)
import Control.Monad (filterM, forM_, forever, guard, when)
import qualified Data.ByteString as BS
import Data.List (sortBy)
import Data.Ord (comparing)
import Network.Simple.TCP (Socket, recv, send)
import SpeedDaemon.Protocol
import SpeedDaemon.Server
import SpeedDaemon.Types

mkClientState :: Socket -> IO ClientState
mkClientState sock = do
  ClientState sock
    <$> newTVarIO Nothing
    <*> newTVarIO Nothing
    <*> newTVarIO BS.empty

startHeartbeat :: ClientState -> Interval -> IO (Async ())
startHeartbeat ClientState{..} interval = async $ forever $ do
  sendMessage clientSocket Heartbeat
  threadDelay $ fromIntegral interval * 100000

handleHeartbeat :: ClientState -> Interval -> IO ()
handleHeartbeat state@ClientState{..} interval = do
  currentInterval <- readTVarIO clientInterval
  case currentInterval of
    Just _ ->
      sendMessage clientSocket $
        ErrorMsg "Cannot set heartbeat interval more than once"
    Nothing -> when (interval > 0) $ do
      heartbeatAsync <- startHeartbeat state interval
      atomically $ do
        writeTVar clientInterval (Just interval)
        writeTVar clientHeartbeatThread (Just heartbeatAsync)

cleanupHeartbeat :: ClientState -> IO ()
cleanupHeartbeat ClientState{..} = do
  maybeAsync <- readTVarIO clientHeartbeatThread
  mapM_ cancel maybeAsync

recvMessages :: ClientState -> IO (Maybe [Message])
recvMessages ClientState{..} = do
  maybeBytes <- recv clientSocket 4096
  case maybeBytes of
    Nothing -> return Nothing
    Just bytes -> do
      buffer <- atomically $ do
        existingBuffer <- readTVar clientBuffer
        let newBuffer = existingBuffer `BS.append` bytes
        writeTVar clientBuffer newBuffer
        return newBuffer

      case parseAllMessages buffer of
        Left _ -> do
          sendMessage clientSocket $ ErrorMsg "Error parsing message"
          return Nothing
        Right (messages, remainingBytes) -> do
          atomically $ writeTVar clientBuffer remainingBytes
          return $ Just messages

sendMessage :: Socket -> Message -> IO ()
sendMessage sock = send sock . encodeMessage

handleClient :: Server -> Socket -> IO ()
handleClient server sock =
  bracket
    (mkClientState sock)
    (handleClientMessages server)
    cleanupHeartbeat

handleClientMessages :: Server -> ClientState -> IO ()
handleClientMessages server state = do
  maybeMsgs <- recvMessages state
  case maybeMsgs of
    Nothing -> return ()
    Just msgs -> do
      handleMessage server state msgs
      handleClientMessages server state

handleMessage :: Server -> ClientState -> [Message] -> IO ()
handleMessage _ _ [] = return ()
handleMessage server state@ClientState{..} (msg : msgs) = do
  case msg of
    WantHeartbeat interval -> do
      handleHeartbeat state interval
      handleMessage server state msgs
    IAmCamera camera -> do
      let client = CameraClient state camera
      atomically $ addCamera server client
      handleCameraClient server client msgs
    IAmDispatcher disp -> do
      let client = DispatcherClient state disp
      atomically $ addDispatcher server client
      handleDispatcherClient server client msgs
    _ -> sendMessage clientSocket $ ErrorMsg "Invalid message"

handleCameraClient :: Server -> CameraClient -> [Message] -> IO ()
handleCameraClient server client@CameraClient{..} msgs = do
  forM_ msgs $ handleCameraMessage server client
  forever $ do
    maybeMsgs <- recvMessages ccState
    case maybeMsgs of
      Nothing -> return ()
      Just newMsgs -> do
        forM_ newMsgs $ handleCameraMessage server client

handleCameraMessage :: Server -> CameraClient -> Message -> IO ()
handleCameraMessage server client@CameraClient{..} = \case
  PlateMsg plate timestamp -> handlePlateMsg server client plate timestamp
  WantHeartbeat interval -> handleHeartbeat ccState interval
  _ -> sendMessage (clientSocket ccState) $ ErrorMsg "Invalid message for camera"

handleDispatcherClient :: Server -> DispatcherClient -> [Message] -> IO ()
handleDispatcherClient server client@DispatcherClient{..} msgs = do
  forM_ (dispatcherRoads dcDispatcher) $ \road -> do
    pendingTickets <- atomically $ do
      tickets <- getPendingTickets server road
      clearRoadTickets server road
      return tickets
    forM_ pendingTickets $ sendMessage (clientSocket dcState) . TicketMsg
  forM_ msgs $ handleDispatcherMessage client
  forever $ do
    maybeMsgs <- recvMessages dcState
    case maybeMsgs of
      Nothing -> return ()
      Just newMsgs -> forM_ newMsgs $ handleDispatcherMessage client

handleDispatcherMessage :: DispatcherClient -> Message -> IO ()
handleDispatcherMessage DispatcherClient{..} = \case
  WantHeartbeat interval -> handleHeartbeat dcState interval
  _ -> sendMessage (clientSocket dcState) $ ErrorMsg "Invalid message for dispatcher"

handlePlateMsg :: Server -> CameraClient -> Plate -> Timestamp -> IO ()
handlePlateMsg server CameraClient{..} plate timestamp = do
  let camera = ccCamera
      road = cameraRoad camera
      mile = cameraMile camera
      limit = cameraLimit camera

  atomically $ addObservation server plate road mile timestamp

  plateObservations <- atomically $ getPlateObservations server plate
  let roadObs = filter ((== road) . obsRoad) plateObservations
      sortedObs = sortBy (comparing obsTimestamp) roadObs

  let possibleViolations = findSpeedingViolations limit sortedObs
  speedingViolations <- atomically $ filterM (checkAndRecordTicket server) possibleViolations

  dispatchers <- atomically $ getRoadDispatchers server road
  case dispatchers of
    [] -> atomically $ forM_ speedingViolations (addTicket server road)
    (dispatcher : _) ->
      forM_
        speedingViolations
        (sendMessage (clientSocket $ dcState dispatcher) . TicketMsg)

checkAndRecordTicket :: Server -> Ticket -> STM Bool
checkAndRecordTicket server ticket = do
  let startDay = ticketTimestamp1 ticket `div` 86400
      endDay = ticketTimestamp2 ticket `div` 86400
      daysSpanned = [startDay .. endDay]

  alreadyTicketed <- hasTicketOnDays server (ticketPlate ticket) daysSpanned
  if alreadyTicketed
    then return False
    else do
      recordTicketDays server (ticketPlate ticket) daysSpanned
      return True

findSpeedingViolations :: Speed -> [Observation] -> [Ticket]
findSpeedingViolations limit observations = do
  obs1 <- observations
  obs2 <- observations
  guard $ obsTimestamp obs1 < obsTimestamp obs2

  let timeDiff = fromIntegral $ obsTimestamp obs2 - obsTimestamp obs1 :: Integer
      distance =
        abs $
          fromIntegral (obsMile obs2) - fromIntegral (obsMile obs1) ::
          Integer
      speed =
        floor
          ((fromIntegral distance * 3600 * 100) / fromIntegral timeDiff :: Float)
      speedingThreshold = fromIntegral limit * 100 + 50 :: Integer

  guard $ speed >= speedingThreshold

  return $
    Ticket
      { ticketPlate = obsPlate obs1
      , ticketRoad = obsRoad obs1
      , ticketMile1 = obsMile obs1
      , ticketTimestamp1 = obsTimestamp obs1
      , ticketMile2 = obsMile obs2
      , ticketTimestamp2 = obsTimestamp obs2
      , ticketSpeed = fromIntegral speed
      }
