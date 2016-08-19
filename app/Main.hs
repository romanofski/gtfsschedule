module Main where

import CSV.Import (parseCSV)
import Database (userDatabaseFile, getLastUpdatedDatabase)
import Database (runDBWithLogging)
import Schedule (printSchedule, getSchedule)
import Message (updateSchedule)
import Update (isDatasetUpToDate, printWarningForNewDataset, isCurrent)

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
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B


-- | Command line options
data Options = Options { stationID :: String
                       , walktime :: Maybe Integer
                       , realtime :: Bool
                       , setup :: Bool
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
               <*> flag False True
               ( long "setup"
               <> short 's'
               <> help "(Development) setup database")

delayFromMaybe ::
  Maybe Integer
  -> Integer
delayFromMaybe = fromMaybe 0

datasetURL :: String
datasetURL = "https://gtfsrt.api.translink.com.au/GTFS/SEQ_GTFS.zip"


runSchedule :: Options -> IO ()
runSchedule (Options sID delay False False) =
  userDatabaseFile >>= \fp -> getSchedule fp sID (delayFromMaybe delay) >>= printSchedule (delayFromMaybe delay)
runSchedule (Options sID delay True False) = do
  let walkDelay = delayFromMaybe delay
  fp <- userDatabaseFile
  d <- getLastUpdatedDatabase (T.pack fp)
  isDatasetUpToDate datasetURL d isCurrent >>= printWarningForNewDataset
  schedule <- getSchedule fp sID walkDelay
  bytes <- simpleHttp "http://gtfsrt.api.translink.com.au/Feed/SEQ"
  case messageGet bytes of
    Left err -> do
      print $ "Error occurred decoding feed: " ++ err
      printSchedule walkDelay schedule
    Right (fm, _) -> do
      printSchedule walkDelay $ updateSchedule schedule fm

main :: IO ()
main = execParser opts >>= runSchedule
  where opts = info (helper <*> optionParser)
               ( fullDesc
                 <> progDesc "Shows schedule of departing vehicles based on static GTFS data."
                 <> header "gtfs - a gtfs enabled transport schedule")
