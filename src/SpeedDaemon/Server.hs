{-# LANGUAGE RecordWildCards #-}

module SpeedDaemon.Server where

import Control.Concurrent.STM
import Control.Monad (forM_)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import SpeedDaemon.Types

mkServer :: IO Server
mkServer =
  Server
    <$> newTVarIO M.empty -- cameras
    <*> newTVarIO M.empty -- dispatchers
    <*> newTVarIO M.empty -- observations
    <*> newTVarIO M.empty -- tickets
    <*> newTVarIO S.empty -- ticket days

addCamera :: Server -> CameraClient -> STM ()
addCamera Server{..} client@CameraClient{..} =
  modifyTVar' cameras $ M.alter (Just . (client :) . fromMaybe []) (cameraRoad ccCamera)

addDispatcher :: Server -> DispatcherClient -> STM ()
addDispatcher Server{..} DispatcherClient{..} =
  forM_ (dispatcherRoads dcDispatcher) $ \road ->
    modifyTVar' dispatchers $
      M.alter
        (Just . (DispatcherClient dcState dcDispatcher :) . fromMaybe [])
        road

addObservation :: Server -> Plate -> Road -> Mile -> Timestamp -> STM ()
addObservation Server{..} plate road mile timestamp =
  modifyTVar' observations $
    M.alter
      (Just . (Observation plate road mile timestamp :) . fromMaybe [])
      plate

getPlateObservations :: Server -> Plate -> STM [Observation]
getPlateObservations Server{..} plate =
  M.findWithDefault [] plate <$> readTVar observations

getRoadDispatchers :: Server -> Road -> STM [DispatcherClient]
getRoadDispatchers Server{..} road =
  M.findWithDefault [] road <$> readTVar dispatchers

getPendingTickets :: Server -> Road -> STM [Ticket]
getPendingTickets Server{..} road =
  M.findWithDefault [] road <$> readTVar tickets

clearRoadTickets :: Server -> Road -> STM ()
clearRoadTickets Server{..} road =
  modifyTVar' tickets $ M.delete road

addTicket :: Server -> Road -> Ticket -> STM ()
addTicket Server{..} road ticket =
  modifyTVar' tickets $
    M.alter
      (Just . (ticket :) . fromMaybe [])
      road

hasTicketOnDays :: Server -> Plate -> [Day] -> STM Bool
hasTicketOnDays Server{..} plate days = do
  ticketed <- readTVar ticketDays
  return $ any (\day -> S.member (TicketDay plate day) ticketed) days

recordTicketDays :: Server -> Plate -> [Day] -> STM ()
recordTicketDays Server{..} plate days =
  modifyTVar' ticketDays $ \s ->
    foldr (S.insert . TicketDay plate) s days
