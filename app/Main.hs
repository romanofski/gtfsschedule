module Main where

import Database (userDatabaseFile, getLastUpdatedDatabase)
import Schedule (printSchedule, getSchedule, getTimeSpecFromNow)
import Message (updateSchedule)
import Update (isDatasetUpToDate, printWarningForNewDataset, isCurrent)
import CSV.Import (createNewDatabase)

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
import Data.Maybe (fromMaybe)
import Network.HTTP.Conduit (simpleHttp)

import Text.ProtocolBuffers (messageGet)
import qualified Data.Text as T


-- | Command line options
data Command = Run { stationID :: String
                   , walktime :: Maybe Integer
                   , realtime :: Bool
                   , setup :: Bool
                   }
             | Setup { logging :: Bool }


optionParser ::
  Parser Command
optionParser = Builder.subparser
               ( Builder.command "run" (info runOptions
                                        (Builder.progDesc "Run gtfsschedule"))
               <> Builder.command "setup" (info setupOptions
                                           (Builder.progDesc "Setup the gtfsschedule database"))
               )

setupOptions ::
  Parser Command
setupOptions = Setup
  <$> Builder.flag False True
  (Builder.long "logging"
    <> help "Enable logging")

runOptions ::
  Parser Command
runOptions = Run
               <$> Builder.argument str
               ( Builder.metavar "STATION"
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


runSchedule :: Command -> IO ()
runSchedule (Setup _) = createNewDatabase datasetURL
runSchedule (Run sID delay False False) = do
  fp <- userDatabaseFile
  timespec <- getTimeSpecFromNow $ delayFromMaybe delay
  getSchedule fp sID timespec >>= printSchedule (delayFromMaybe delay)
runSchedule (Run sID delay True False) = do
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
                 <> Builder.header "gtfs - a gtfs enabled transport schedule")
