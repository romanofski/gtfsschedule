module Main where

import Database (userDatabaseFile, getLastUpdatedDatabase)
import Schedule (printSchedule, getSchedule, getTimeSpecFromNow)
import Message (updateSchedule)
import Update (isDatasetUpToDate, printWarningForNewDataset, isCurrent)
import CSV.Import (createNewDatabase)

import Data.Functor ((<$>))
import Control.Applicative ((<*>))

import qualified Options.Applicative.Builder as Builder
import Options.Applicative.Builder (long
                                   , help
                                   , (<>)
                                   , option
                                   , flag
                                   , str
                                   , info
                                   , short
                                   , auto)
import Options.Applicative (optional)
import Options.Applicative.Types (Parser)
import Options.Applicative.Extra ( execParser
                                 , helper)
import Data.Version (showVersion)
import Paths_gtfsschedule (version)
import Data.Maybe (fromMaybe)
import Network.HTTP.Conduit (simpleHttp)

import Text.ProtocolBuffers (messageGet)
import qualified Data.Text as T


-- | Command line options
data Command
    = Monitor { stationID :: String
              , walktime :: Maybe Integer
              , realtime :: Bool
              }
    | Setup { logging :: Bool}


optionParser ::
  Parser Command
optionParser =
    Builder.subparser
        (Builder.command
             "monitor"
             (info
                  monitorOptions
                  (Builder.progDesc
                       "Monitor a stop/station and print next departing services")) <>
         Builder.command
             "setup"
             (info
                  setupOptions
                  (Builder.progDesc "Setup the gtfsschedule database")))

setupOptions ::
  Parser Command
setupOptions = Setup
  <$> Builder.flag False True
  (Builder.long "logging"
    <> help "Enable logging")

monitorOptions ::
  Parser Command
monitorOptions =
    Monitor <$>
    Builder.argument
        str
        (Builder.metavar "STATION" <>
         help "Station id to show the schedule for") <*>
    optional
        (option
             auto
             (long "walktime" <>
              help
                  "Time to reach the stop. Will be added to the current time to allow arriving at the station just in time for departure.")) <*>
    flag
        False
        True
        (long "realtime" <> short 'r' <> help "Enable realtime updates")

delayFromMaybe ::
  Maybe Integer
  -> Integer
delayFromMaybe = fromMaybe 0

datasetURL :: String
datasetURL = "https://gtfsrt.api.translink.com.au/GTFS/SEQ_GTFS.zip"


programHeader :: String
programHeader =
  "gtfsschedule - Be on time for your next public transport service (v. " ++ showVersion version ++ ")"

runSchedule :: Command -> IO ()
runSchedule (Setup _) = userDatabaseFile >>= createNewDatabase datasetURL
runSchedule (Monitor sID delay False) = do
  fp <- userDatabaseFile
  timespec <- getTimeSpecFromNow $ delayFromMaybe delay
  getSchedule fp sID timespec >>= printSchedule (delayFromMaybe delay)
runSchedule (Monitor sID delay True) = do
  let walkDelay = delayFromMaybe delay
  fp <- userDatabaseFile
  d <- getLastUpdatedDatabase (T.pack fp)
  isDatasetUpToDate datasetURL d isCurrent >>= printWarningForNewDataset
  timespec <- getTimeSpecFromNow walkDelay
  schedule <- getSchedule fp sID timespec
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
               ( Builder.fullDesc
                 <> Builder.progDesc "Shows schedule of departing vehicles based on static GTFS data."
                 <> Builder.header programHeader)
