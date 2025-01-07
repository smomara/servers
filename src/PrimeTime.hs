{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module PrimeTime where

import Data.Aeson
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.ByteString.Char8 (ByteString, isSuffixOf, lines)
import GHC.Generics (Generic)
import Network.Simple.TCP
import Control.Monad (when)
import Data.Numbers.Primes (isPrime)

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

isPrime' :: Double -> Bool
isPrime' n
  | n /= fromIntegral m = False
  | otherwise = isPrime m
 where
  m = truncate n :: Int

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

recvAll :: Socket -> ByteString -> IO ByteString
recvAll sock acc = do
  mbytes <- recv sock 4096
  case mbytes of
    Nothing -> return acc
    Just bytes -> do
      let newAcc = acc <> bytes
      if "\n" `isSuffixOf` newAcc
        then return newAcc
        else recvAll sock newAcc

handleClient :: Socket -> IO ()
handleClient sock = do
  bytes <- recvAll sock ""
  let requestLines = Data.ByteString.Char8.lines bytes
  continue <- processLines sock requestLines
  when continue (handleClient sock)

processLines :: Socket -> [ByteString] -> IO Bool
processLines _ [] = return True
processLines sock (line : rest) = do
  let request = decode (fromStrict line) :: Maybe Request
  case request of
    Nothing -> do
      sendResponse sock malformedResponse
      return False
    Just req -> do
      if reqMethod req /= "isPrime"
        then do
          sendResponse sock malformedResponse
          return False
        else do
          let result = isPrime' (reqNumber req)
          sendResponse sock (validResponse result)
          processLines sock rest

main :: IO ()
main = do
  let port = "44444"
  runPrimeTime port
