-- | the GTFS schedule
module Schedule
    (printSchedule) where

import Data.Time.LocalTime ( utcToLocalTime
                           , LocalTime(..)
                           , timeOfDayToTime
                           , timeToTimeOfDay)
import Data.Time (getCurrentTime, getCurrentTimeZone)
import Data.Time.Clock (secondsToDiffTime)
import Control.Monad.IO.Class (liftIO)
import qualified Database as DB
import qualified Data.Text as T


-- | prints list of StopTimes as schedule
--
printSchedule ::
  String
  -> String
  -> Integer
  -> IO ()
printSchedule sqliteDBFilepath sId delay = DB.runDBWithoutLogging (T.pack sqliteDBFilepath) $ do
      t <- liftIO getCurrentTime
      tz <- liftIO getCurrentTimeZone
      let delaySeconds = secondsToDiffTime (delay * 60)
      let (LocalTime lDay lTimeOfDay) = utcToLocalTime tz t
      let lTimeWithDelay = timeToTimeOfDay $ timeOfDayToTime lTimeOfDay + delaySeconds
      stops <- DB.getNextDepartures sId lTimeWithDelay lDay
      liftIO $ putStr $ DB.printStopTimesAsSchedule lTimeOfDay delaySeconds stops
