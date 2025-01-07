module Main where

import System.Environment (getArgs)
import Text.Read (readMaybe)
import PrimeTime (runPrimeTime)
import SmokeTest (runSmokeTest)
import MeansToAnEnd (runMeansToAnEnd)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [serverType, port] -> handleServer serverType port
    _ -> putStrLn "Usage: server-selector <primetime|smoketest|meanstoanend> <port>"

handleServer :: String -> String -> IO ()
handleServer serverType portStr =
  case validatePort portStr of
    Just validPort ->
      case serverType of
        "primetime" -> runPrimeTime validPort
        "smoketest" -> runSmokeTest validPort
        "meanstoanend" -> runMeansToAnEnd validPort
        _ -> putStrLn "Usage: server-selector <primetime|smoketest|meanstoanend> <port>"
    Nothing -> putStrLn "Error: Port must be a number between 1 and 65535."

validatePort :: String -> Maybe String
validatePort portStr = do
  port <- readMaybe portStr :: Maybe Int
  if port >= 1 && port <= 65535
    then Just portStr
    else Nothing
