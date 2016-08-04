module Main where

import Schedule (printSchedule, getSchedule)
import Message (updateSchedule)

import Options.Applicative.Builder (long
                                   , help
                                   , (<>)
                                   , metavar
                                   , option
                                   , flag
                                   , argument
                                   , str
                                   , fullDesc
                                   , progDesc
                                   , header
                                   , info
                                   , short
                                   , auto)
import Options.Applicative (optional)
import Options.Applicative.Types (Parser)
import Options.Applicative.Extra ( execParser
                                 , helper)
import Data.Maybe (fromMaybe)
import Network.HTTP.Conduit (simpleHttp)

import Text.ProtocolBuffers (messageGet)

import System.Environment.XDG.BaseDir (getUserDataFile)


-- | Command line options
data Options = Options { stationID :: String
                       , walktime :: Maybe Integer
                       , realtime :: Bool
                       }


optionParser ::
  Parser Options
optionParser = Options
               <$> argument str
               ( metavar "STATION"
                 <> help "Station id to show the schedule for")
               <*> optional
               (option auto
               ( long "walktime"
                 <> help "Time to reach the stop. Will be added to the current time to allow arriving at the station just in time for departure."))
               <*> flag False True
               ( long "realtime"
               <> short 'r'
               <> help "Enable realtime updates")

databaseFile ::
  IO String
databaseFile = getUserDataFile "gtfs" "gtfs.sqlite"

delayFromMaybe ::
  Maybe Integer
  -> Integer
delayFromMaybe = fromMaybe 0

runSchedule :: Options -> IO ()
runSchedule (Options sID delay False) =
  databaseFile >>= \fp -> getSchedule fp sID (delayFromMaybe delay) >>= printSchedule (delayFromMaybe delay)
runSchedule (Options sID delay True) = do
  let walkDelay = delayFromMaybe delay
  fp <- databaseFile
  schedule <- getSchedule fp sID walkDelay
  bytes <- simpleHttp "http://gtfsrt.api.translink.com.au/Feed/SEQ"
  case messageGet bytes of
    Left err -> do
      print $ "Error occurred decoding feed: " ++ err
      printSchedule walkDelay schedule
    Right (fm, _) -> printSchedule walkDelay $ updateSchedule schedule fm

main :: IO ()
main = execParser opts >>= runSchedule
  where opts = info (helper <*> optionParser)
               ( fullDesc
                 <> progDesc "Shows schedule of departing vehicles based on static GTFS data."
                 <> header "gtfs - a gtfs enabled transport schedule")
