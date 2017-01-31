{-# LANGUAGE RecordWildCards #-}

module Main where

import Config (loadGTFSConfig, withConfigfile, Command(..))

import GTFS.Schedule
import GTFS.Database (userDatabaseFile)
import GTFS.Realtime.Message (updateSchedulesWithRealtimeData)
import GTFS.Realtime.Update (isDatasetUpToDate, printOrUpdateDataset, isCurrent, Error)
import CSV.Import (createNewDatabase)

import Control.Applicative ((<$>), (<*>), (<|>), pure, some, optional)
import Data.List (findIndex)
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)

import Control.Monad.Reader (ask, local)

import Data.Ini (Ini)

import qualified Options.Applicative.Builder as Builder
import Options.Applicative.Builder (
  long, help, argument, metavar, flag, str, info, short, auto)
import Options.Applicative.Types (Parser, ReadM(..))
import Options.Applicative.Extra ( execParser
                                 , helper)
import Data.Version (showVersion)
import Paths_gtfsschedule (version)

import qualified Data.Text as T


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


optionParser ::
  Ini -> Parser Command
optionParser conf =
    Builder.subparser
        (Builder.command
             "monitor"
             (info
                  (monitorOptions conf)
                  (Builder.progDesc
                       "Monitor a stop/station and print next departing services")) <>
         Builder.command
             "setup"
             (info
                  (setupOptions conf)
                  (Builder.progDesc "Setup the gtfsschedule database")))

setupOptions ::
  Ini -> Parser Command
setupOptions conf = Setup
  <$> (optional $ Builder.option txtReader ( long "static-url" <> withConfigfile conf (T.pack "static-url") <> metavar "URL" <> short 's' <> help "URL to the static dataset zip archive" ))

txtReader :: ReadM T.Text
txtReader = ReadM $ do
  s <- ask
  return $ T.pack s

monitorOptions :: Ini -> Parser Command
monitorOptions conf =
  Monitor
  <$> some (
    argument stopWithWalktime
      ( metavar "STATION-ID[:MINUTES]"
      <> help "Station id to show the schedule for, and optional walktime in minutes to that station (default: 0))"
      )
    )
  <*> flag False True
    (long "realtime" <> short 'r' <> help "Enable realtime updates")
  <*> flag False True
    (long "autoupdate" <> short 'u' <> help "Automatically update the static GTFS dataset")
  <*> (optional $ Builder.option txtReader ( long "static-url" <> withConfigfile conf (T.pack "static-url") <> metavar "URL" <> help "URL to the static dataset zip archive" ))
  <*> (optional $ Builder.option txtReader ( long "realtime-url" <> withConfigfile conf (T.pack "realtime-url") <> metavar "URL" <> help "URL to the realtime GTFS feed" ))


programHeader :: String
programHeader =
  "gtfsschedule - Be on time for your next public transport service (v. " ++ showVersion version ++ ")"


runSchedule :: String -> Command -> IO ()
runSchedule dbfile (Setup (Just url)) = createNewDatabase (T.unpack url) dbfile
runSchedule _ (Setup Nothing) = error "The static-url must be set either by command line argument or configuration file."
runSchedule dbfile (Monitor {..}) = do
    printOrUpdateDataset autoUpdate staticDatasetURL
    schedules <-
        getSchedulesByWalktime dbfile stopsWithWalktime >>=
        updateSchedulesWithRealtimeData realtimeFeedURL
    let schedule = take 3 $ sortSchedules schedules
    printSchedule schedule =<< getCurrentTimeOfDay

main :: IO ()
main = do
  fp <- userDatabaseFile
  conf <- loadGTFSConfig
  execParser (opts conf) >>= runSchedule fp
    where opts c = info (helper <*> optionParser c)
               ( Builder.fullDesc
                 <> Builder.progDesc "Shows schedule of departing vehicles based on static GTFS data."
                 <> Builder.header programHeader)
