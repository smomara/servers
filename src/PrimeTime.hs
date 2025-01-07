{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module PrimeTime where

import Data.Aeson
import Data.ByteString.Lazy (fromStrict, toStrict)
import GHC.Generics (Generic)
import Network.Simple.TCP

data Request = Request
  { reqMethod :: String
  , reqNumber :: Double
  }
  deriving (Show, Generic)

instance FromJSON Request where
  parseJSON = withObject "Request" $ \v ->
    Request
      <$> v .: "method"
      <*> v .: "number"

data Response = Response
  { resMethod :: String
  , resPrime :: Bool
  }
  deriving (Show, Generic)

instance ToJSON Response where
  toJSON (Response method prime) =
    object
      [ "method" .= method
      , "prime" .= prime
      ]

isPrime :: Double -> Bool
isPrime n
  | n /= fromIntegral m = False
  | otherwise = prime m
 where
  m = truncate n :: Int
  prime num
    | num < 2 = False
    | num == 2 = True
    | even num = False
    | otherwise = null divisors
   where
    divisors = do
      x <- [3, 5 .. floor (sqrt (fromIntegral num :: Double))]
      if num `mod` x == 0 then pure x else []

runPrimeTime :: ServiceName -> IO ()
runPrimeTime port = serve (Host "0.0.0.0") port $ \(sock, _) -> handleClient sock

validResponse :: Bool -> Response
validResponse result = Response{resMethod = "isPrime", resPrime = result}

malformedResponse :: Response
malformedResponse = Response{resMethod = "malformed", resPrime = False}

sendResponse :: Socket -> Response -> IO ()
sendResponse sock response = do
  let responseBytes = toStrict $ encode response
  send sock (responseBytes <> "\n")

handleClient :: Socket -> IO ()
handleClient sock = do
  -- TODO: make able to handle message until ends
  -- message may be greater than 4096
  -- might involve making a recvUntil function in the library
  -- recvUntil :: (MonadIO m, Ex.MonadMask m)
  --           => NS.Socket                   -- ^ Socket to receive data from.
  --           -> Int                         -- ^ Maximum chunk size to receive at once.
  --           -> (BS.ByteString -> Bool)     -- ^ Predicate to check if the received data meets the condition.
  --           -> m BS.ByteString             -- ^ Accumulated data received.
  -- recvUntil sock chunkSize predicate = go BS.empty
  --   where
  --     go :: BS.ByteString -> m BS.ByteString
  --     go acc = do
  --       mchunk <- recv sock chunkSize
  --       case mchunk of
  --         Nothing -> return acc  -- Remote end closed the connection.
  --         Just chunk -> do
  --           let newAcc = BS.append acc chunk
  --           if predicate newAcc
  --             then return newAcc
  --             else go newAcc
  mbytes <- recv sock 4096
  case mbytes of
    Nothing -> return ()
    Just bytes -> do
      let request = decode (fromStrict bytes) :: Maybe Request
      case request of
        Nothing -> sendResponse sock malformedResponse
        Just req -> do
          if reqMethod req /= "isPrime"
            then sendResponse sock malformedResponse
            else do
              let primeCheck = isPrime (reqNumber req)
              sendResponse sock (validResponse primeCheck)
              handleClient sock

main :: IO ()
main = do
  let port = "44444"
  runPrimeTime port
