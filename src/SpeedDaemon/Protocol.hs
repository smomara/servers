{-# LANGUAGE RecordWildCards #-}

module SpeedDaemon.Protocol where

import Control.Monad (replicateM)
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import SpeedDaemon.Types
import Data.List (isInfixOf)

parseStr :: Get Str
parseStr = do
  len <- getWord8
  BS.pack <$> replicateM (fromIntegral len) getWord8

parseRoads :: Get [Road]
parseRoads = do
  numRoads <- getWord8
  replicateM (fromIntegral numRoads) getWord16be

parseCamera :: Get Camera
parseCamera =
  Camera
    <$> getWord16be -- road
    <*> getWord16be -- mile
    <*> getWord16be -- limit

parseTicket :: Get Ticket
parseTicket =
  Ticket
    <$> parseStr -- plate
    <*> getWord16be -- road
    <*> getWord16be -- mile1
    <*> getWord32be -- timestamp1
    <*> getWord16be -- mile2
    <*> getWord32be -- timestamp2
    <*> getWord16be -- speed

parseMessage :: Get Message
parseMessage = do
  msgType <- getWord8
  case msgType of
    0x10 -> ErrorMsg <$> parseStr
    0x20 -> PlateMsg <$> parseStr <*> getWord32be
    0x21 -> TicketMsg <$> parseTicket
    0x40 -> WantHeartbeat <$> getWord32be
    0x41 -> return Heartbeat
    0x80 -> IAmCamera <$> parseCamera
    0x81 -> IAmDispatcher . Dispatcher <$> parseRoads
    _ -> fail "Unknown message type"

parseAllMessages :: ByteString -> Either String ([Message], ByteString)
parseAllMessages bs | BS.null bs = Right ([], BS.empty)
parseAllMessages bs = case runGetOrFail parseMessage (BL.fromStrict bs) of
  Left (_, _, err) -> do
    if "not enough bytes" `isInfixOf` err
      then Right ([], bs)
      else Left err
  Right (rest, _, msg) -> do
    case parseAllMessages (BL.toStrict rest) of
      Left err -> Left err
      Right (msgs, remainingBytes) -> Right (msg : msgs, remainingBytes)

-----------
-- ENCODING
serializeByteString :: ByteString -> Put
serializeByteString bs = do
  putWord8 $ fromIntegral (BS.length bs)
  putByteString bs

serializeRoads :: [Road] -> Put
serializeRoads roads = do
  putWord8 (fromIntegral $ length roads)
  mapM_ putWord16be roads

serializeCamera :: Camera -> Put
serializeCamera Camera{..} = do
  putWord16be cameraRoad
  putWord16be cameraMile
  putWord16be cameraLimit

serializeTicket :: Ticket -> Put
serializeTicket Ticket{..} = do
  serializeByteString ticketPlate
  putWord16be ticketRoad
  putWord16be ticketMile1
  putWord32be ticketTimestamp1
  putWord16be ticketMile2
  putWord32be ticketTimestamp2
  putWord16be ticketSpeed

serializeMessage :: Message -> Put
serializeMessage msg = case msg of
  ErrorMsg bs -> do
    putWord8 0x10
    serializeByteString bs
  PlateMsg plate timestamp -> do
    putWord8 0x20
    serializeByteString plate
    putWord32be timestamp
  TicketMsg ticket -> do
    putWord8 0x21
    serializeTicket ticket
  WantHeartbeat interval -> do
    putWord8 0x40
    putWord32be interval
  Heartbeat ->
    putWord8 0x41
  IAmCamera camera -> do
    putWord8 0x80
    serializeCamera camera
  IAmDispatcher (Dispatcher roads) -> do
    putWord8 0x81
    serializeRoads roads

encodeMessage :: Message -> ByteString
encodeMessage = BL.toStrict . runPut . serializeMessage
