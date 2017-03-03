{-# LANGUAGE RecordWildCards #-}

module CLI.Main where

import CLI.Config
import GTFS.Schedule
       (getSchedulesByWalktime, printSchedule, getCurrentTimeOfDay,
        sortSchedules, ScheduleConfig(..), defaultScheduleItemTemplate)
import GTFS.Database (userDatabaseFile, searchStopCode, StopSearchResult(..))
import GTFS.Realtime.Update (printOrUpdateDataset)
import GTFS.Realtime.Message.Schedule (updateSchedulesWithRealtimeData)
import CSV.Import (createNewDatabase)

import Options.Applicative.Extra (execParser, helper)
import qualified Options.Applicative.Builder as Builder

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Control.Applicative ((<*>))
import qualified Data.Text as T


runSchedule :: String -> Command -> IO ()
runSchedule dbfile (Search str) = do
  results <- searchStopCode (T.pack dbfile) str
  mapM_ (\r -> putStrLn $ "" ++ resultStopcode r ++ " " ++ resultStopName r) results
  putStrLn $ "Found " ++ show (length results) ++ " matches"
runSchedule dbfile (Setup (Just url)) = createNewDatabase (T.unpack url) dbfile
runSchedule _ (Setup Nothing) = error "The static-url must be set either by command line argument or configuration file."
runSchedule dbfile (Monitor{..}) = do
    let l = fromMaybe 3 limit
    printOrUpdateDataset autoUpdate staticDatasetURL
    schedules <-
        getSchedulesByWalktime dbfile l stops >>=
        updateSchedulesWithRealtimeData realtimeFeedURL
    let schedule = take (fromInteger l) $ sortSchedules schedules
    tod <- getCurrentTimeOfDay
    printSchedule schedule (ScheduleConfig tod $ fromMaybe defaultScheduleItemTemplate scheduleItemTemplate)

-- | Wrapper which uses the configuration files to configure, parse the command
-- line options and finally run the application
runCLI :: String -> String -> IO ()
runCLI header desc = do
  fp <- userDatabaseFile
  conf <- loadGTFSConfig
  execParser (opts conf) >>= runSchedule fp
    where opts c = Builder.info (helper <*> optionParser c)
               ( Builder.fullDesc
                 <> Builder.progDesc desc
                 <> Builder.header header)
