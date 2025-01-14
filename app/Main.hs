module Main where

import qualified Data.Map as M
import Data.List (intercalate)
import System.Environment (getArgs)
import Text.Read (readMaybe)
import PrimeTime (runPrimeTime)
import SmokeTest (runSmokeTest)
import MeansToAnEnd (runMeansToAnEnd)
import BudgetChat (runBudgetChat)
import MobInTheMiddle (runMobInTheMiddle)
import SpeedDaemon (runSpeedDaemon)
import InsecureSocketsLayer (runInsecureSocketsLayer)

serverHandlers :: M.Map String (String -> IO ())
serverHandlers =
  M.fromList
    [ ("primetime", runPrimeTime),
      ("smoketest", runSmokeTest),
      ("meanstoanend", runMeansToAnEnd),
      ("budgetchat", runBudgetChat),
      ("mobinthemiddle", runMobInTheMiddle),
      ("speeddaemon", runSpeedDaemon),
      ("insecuresocketslayer", runInsecureSocketsLayer)
    ]

usage :: String
usage = "Usage: server-selector <" ++ intercalate "|" (M.keys serverHandlers) ++ "> <port>"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [serverType, portStr] -> handleServer serverType portStr
    _ -> putStrLn usage

handleServer :: String -> String -> IO ()
handleServer serverType portStr =
  case validatePort portStr of
    Just validPort ->
      case M.lookup serverType serverHandlers of
        Just handler -> handler validPort
        Nothing -> putStrLn usage
    Nothing -> putStrLn "Error: Port must be a number between 1 and 65535."

validatePort :: String -> Maybe String
validatePort portStr = do
  port <- readMaybe portStr :: Maybe Int
  if port >= 1 && port <= 65535
    then Just portStr
    else Nothing
