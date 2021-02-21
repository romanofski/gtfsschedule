{-# LANGUAGE RecordWildCards #-}
{-
Copyright (C) - 2017 Róman Joost <roman@bromeco.de>

This file is part of gtfsschedule.

gtfsschedule is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

gtfsschedule is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with gtfsschedule.  If not, see <http://www.gnu.org/licenses/>.
-}
module CLI.Main where

import           CLI.Config
import           CSV.Import                     (createNewDatabase)
import           GTFS.Database                  (StopSearchResult (..),
                                                 searchStopCode,
                                                 userDatabaseFile)
import           GTFS.Realtime.Message.Schedule (updateSchedulesWithRealtimeData)
import           GTFS.Realtime.Update           (printOrUpdateDataset)
import           GTFS.Schedule                  (ScheduleConfig (..),
                                                 defaultScheduleItemTemplate,
                                                 getCurrentTimeOfDay,
                                                 getSchedulesByWalktime,
                                                 printSchedule, sortSchedules)

import qualified Options.Applicative.Builder    as Builder
import           Options.Applicative.Extra      (execParser, helper)

import           Data.Maybe                     (fromMaybe)
import qualified Data.Text                      as T
import           System.IO                      (stdout)


runSchedule :: String -> Command -> IO ()
runSchedule dbfile (Search str) = do
  results <- searchStopCode (T.pack dbfile) str
  mapM_ (\r -> putStrLn $ "" ++ resultStopcode r ++ " " ++ resultStopName r) results
  putStrLn $ "Found " ++ show (length results) ++ " matches"
runSchedule dbfile (Setup (Just url)) = createNewDatabase (T.unpack url) dbfile
runSchedule _ (Setup Nothing) = error "The static-url must be set either by command line argument or configuration file."
runSchedule dbfile Monitor{..} = do
    let l = fromMaybe 3 limit
    printOrUpdateDataset autoUpdate staticDatasetURL
    schedules <-
        getSchedulesByWalktime dbfile l stops >>=
        updateSchedulesWithRealtimeData realtimeFeedURL
    let schedule = take (fromInteger l) $ sortSchedules schedules
    tod <- getCurrentTimeOfDay
    printSchedule schedule (ScheduleConfig tod $ fromMaybe defaultScheduleItemTemplate scheduleItemTemplate) stdout

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
