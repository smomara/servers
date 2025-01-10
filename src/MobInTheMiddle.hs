{-# LANGUAGE OverloadedStrings #-}

module MobInTheMiddle (runMobInTheMiddle) where

import Control.Concurrent.Async (race)
import Control.Monad (void)
import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Simple.TCP

tonyAddress :: Text
tonyAddress = "7YWHMfk9JZe0LM0g1ZauHuiSxhI"

runMobInTheMiddle :: ServiceName -> IO ()
runMobInTheMiddle port = serve (Host "0.0.0.0") port $ \(clientSock, _) -> do
  connect "chat.protohackers.com" "16963" $ \(upstreamSock, _) -> do
    void $ race (forward clientSock upstreamSock) (forward upstreamSock clientSock)

forward :: Socket -> Socket -> IO ()
forward srcSock dstSock = do
  maybeBytes <- recv srcSock 4096
  case maybeBytes of
    Nothing -> return ()
    Just bytes -> do
      let msg = TE.decodeUtf8 bytes
      mapM_ (send dstSock . TE.encodeUtf8 . (<> "\n") . rewriteAddresses) (T.lines msg)
      forward srcSock dstSock

rewriteAddresses :: Text -> Text
rewriteAddresses msg = T.unwords $ map rewriteWord (T.words msg)
 where
  rewriteWord word
    | isAddress word = tonyAddress
    | otherwise = word

isAddress :: Text -> Bool
isAddress msg =
  T.head msg == '7'
    && 26 <= T.length msg
    && T.length msg <= 35
    && T.all isAlphaNum msg
