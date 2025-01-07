module MeansToAnEnd (runMeansToAnEnd) where

import Control.Concurrent.STM
import Data.Binary.Get (Get, getInt32be, getWord8, runGet)
import Data.Binary.Put (putInt32be, runPut)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int32)
import Data.Map (Map)
import qualified Data.Map as M
import Network.Simple.TCP

type Prices = Map Int32 Int32

data Message = Insert Int32 Int32 | Query Int32 Int32

runMeansToAnEnd :: ServiceName -> IO ()
runMeansToAnEnd port = serve (Host "0.0.0.0") port $ \(sock, _) -> do
  pricesVar <- newTVarIO M.empty
  handleClient sock pricesVar

handleClient :: Socket -> TVar Prices -> IO ()
handleClient sock pricesVar = go BS.empty
 where
  go buffer = do
    chunk <- recv sock 1024
    case chunk of
      Nothing -> return ()
      Just bs -> processMessages (BS.append buffer bs)

  processMessages buffer
    | BS.length buffer < 9 = go buffer
    | otherwise = do
        let (msg, rest) = BS.splitAt 9 buffer
        case decodeMessage msg of
          Just (Insert timestamp price) -> do
            atomically $ modifyTVar pricesVar (M.insert timestamp price)
            processMessages rest
          Just (Query mintime maxtime) -> do
            avg <- atomically $ computeAverage pricesVar mintime maxtime
            send sock $ encodeResponse avg
            processMessages rest
          Nothing -> return ()

computeAverage :: TVar Prices -> Int32 -> Int32 -> STM Int32
computeAverage pricesVar mintime maxtime = do
  pricesMap <- readTVar pricesVar
  let filtered = M.filterWithKey (\t _ -> t >= mintime && t <= maxtime) pricesMap
  if M.null filtered
    then return 0
    else
      let values = M.elems filtered
          sum' = sum (map fromIntegral values :: [Integer])
          len = fromIntegral (length values) :: Integer
       in return $ fromIntegral (sum' `div` len)

decodeMessage :: ByteString -> Maybe Message
decodeMessage bs = runGet parser (BL.fromStrict bs)
 where
  parser :: Get (Maybe Message)
  parser = do
    msgType <- getWord8
    case msgType of
      0x49 -> Just <$> (Insert <$> getInt32be <*> getInt32be)
      0x51 -> Just <$> (Query <$> getInt32be <*> getInt32be)
      _ -> return Nothing

encodeResponse :: Int32 -> ByteString
encodeResponse = BL.toStrict . runPut . putInt32be
