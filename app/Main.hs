{-# LANGUAGE RecordWildCards #-}

module Main where

import Database (userDatabaseFile, getLastUpdatedDatabase)
import Schedule (printSchedule, getSchedule, getTimeSpecFromNow)
import Message (updateSchedule)
import Update (isDatasetUpToDate, printWarningForNewDataset, isCurrent)
import CSV.Import (createNewDatabase)

import Control.Applicative ((<$>), (<*>), (<|>), pure, some)
import Data.List (findIndex)
import Data.Monoid ((<>))

import Control.Monad.Reader (ask, local)

import qualified Options.Applicative.Builder as Builder
import Options.Applicative.Builder (
  long, help, argument, metavar, flag, str, info, short, auto)
import Options.Applicative.Types (Parser, ReadM(..))
import Options.Applicative.Extra ( execParser
                                 , helper)
import Data.Version (showVersion)
import Paths_gtfsschedule (version)
import Network.HTTP.Conduit (simpleHttp)

import Text.ProtocolBuffers (messageGet)
import qualified Data.Text as T


type StopWithWalktime = (String, Integer)

-- | optparse-applicative reader for stop/walktime pair
--
stopWithWalktime :: ReadM StopWithWalktime
stopWithWalktime = ReadM $ do
  s <- ask
  let
    i = findIndex (== '+') s
    stopId = maybe s (`take` s) i
    walktime = maybe "" ((`drop` s) . (+1)) i
  (,)
    <$> local (const stopId) (unReadM str)
    <*> (local (const walktime) (unReadM auto) <|> pure 0)


-- | Command line options
data Command
    = Monitor { stopsWithWalktime :: [StopWithWalktime]
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
  Monitor
  <$> some (
    argument stopWithWalktime
      ( metavar "STATION-ID[:MINUTES]"
      <> help "Station id to show the schedule for, and optional walktime in minutes to that station (default: 0))"
      )
    )
  <*> flag False True
    (long "realtime" <> short 'r' <> help "Enable realtime updates")


datasetURL :: String
datasetURL = "https://gtfsrt.api.translink.com.au/GTFS/SEQ_GTFS.zip"


programHeader :: String
programHeader =
  "gtfsschedule - Be on time for your next public transport service (v. " ++ showVersion version ++ ")"

runSchedule :: Command -> IO ()
runSchedule (Setup _) = userDatabaseFile >>= createNewDatabase datasetURL
runSchedule (Monitor{..}) = do
  let (sID, walkDelay) = head stopsWithWalktime
  timespec <- getTimeSpecFromNow walkDelay
  fp <- userDatabaseFile
  schedule <- getSchedule fp sID timespec
  schedule' <- if realtime
    then do
      d <- getLastUpdatedDatabase (T.pack fp)
      isDatasetUpToDate datasetURL d isCurrent >>= printWarningForNewDataset
      bytes <- simpleHttp "http://gtfsrt.api.translink.com.au/Feed/SEQ"
      case messageGet bytes of
        Left err -> do
          print $ "Error occurred decoding feed: " ++ err
          pure schedule
        Right (fm, _) -> do
          pure $ updateSchedule schedule fm
    else
      pure schedule
  printSchedule walkDelay schedule'


main :: IO ()
main = execParser opts >>= runSchedule
  where opts = info (helper <*> optionParser)
               ( Builder.fullDesc
                 <> Builder.progDesc "Shows schedule of departing vehicles based on static GTFS data."
                 <> Builder.header programHeader)
