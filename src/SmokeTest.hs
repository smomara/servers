module SmokeTest (runSmokeTest) where

import Network.Simple.TCP

runSmokeTest :: ServiceName -> IO ()
runSmokeTest port = serve (Host "0.0.0.0") port $ \(sock, _) -> handleClient sock

handleClient :: Socket -> IO ()
handleClient sock = do
  mbytes <- recv sock 4096
  case mbytes of
    Nothing -> return ()
    Just bytes -> send sock bytes >> handleClient sock
