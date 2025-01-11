module SpeedDaemon where

import Network.Simple.TCP
import SpeedDaemon.Server
import SpeedDaemon.Client

runSpeedDaemon :: ServiceName -> IO ()
runSpeedDaemon port = do
  server <- mkServer
  serve (Host "0.0.0.0") port $ \(sock, _) -> handleClient server sock
