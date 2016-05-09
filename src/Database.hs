{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE PatternGuards              #-}
module Database where

import Data.Time.LocalTime ( TimeOfDay(..)
                           , timeOfDayToTime
                           , timeToTimeOfDay)
import Data.Time.Clock (secondsToDiffTime)
import Data.Time.Calendar ( Day(..))
import Data.Time.Format ( formatTime
                        , defaultTimeLocale)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Control.Monad.Logger (NoLoggingT(..), runNoLoggingT)
import Database.Persist.TH
import Database.Esqueleto
import Data.List (stripPrefix)
import qualified Database.Persist.Sqlite as Sqlite

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
StopTime
    trip TripId
    arrivalTime TimeOfDay
    departureTime TimeOfDay
    stop String
    stopSequence String
    stopHeadsign String Maybe
    pickupType Int Maybe
    dropOffType Int Maybe
    shapeDistTravel Int Maybe
    timepoint Int Maybe
    deriving Show
Trip
    routeId String
    serviceId String
    headsign String Maybe
    shortName String Maybe
    directionId Bool Maybe
    blockId String Maybe
    shapeId String Maybe
    wheelchairAccessible Int Maybe
    bikesAllowed Int Maybe
    deriving Show
Calendar
    serviceId String
    monday Bool
    tuesday Bool
    wednesday Bool
    thursday Bool
    friday Bool
    saturday Bool
    sunday Bool
    startDate Day
    endDate Day
|]

weekdayToSQLExp ::
  String
  -> EntityField Calendar Bool
weekdayToSQLExp weekday
  | Just _ <- stripPrefix "Monday" weekday = CalendarMonday
  | Just _ <- stripPrefix "Tuesday" weekday = CalendarTuesday
  | Just _ <- stripPrefix "Wednesday" weekday = CalendarWednesday
  | Just _ <- stripPrefix "Thursday" weekday = CalendarThursday
  | Just _ <- stripPrefix "Friday" weekday = CalendarFriday
  | Just _ <- stripPrefix "Saturday" weekday = CalendarSaturday
  | Just _ <- stripPrefix "Sunday" weekday = CalendarSunday
  | otherwise = error "That should never happen"

getNextDepartures ::
  String
  -> TimeOfDay
  -> Day
  -> ReaderT Sqlite.SqlBackend (NoLoggingT (ResourceT IO)) [Sqlite.Entity StopTime]
getNextDepartures stopID now nowDate = select $ from $ \(s, t, c) -> do
  where_ (s ^. StopTimeStop ==. val stopID &&.
          s ^. StopTimeDepartureTime >. val earliest &&.
          s ^. StopTimeDepartureTime <. val latest &&.
          s ^. StopTimeTrip ==. t ^. TripId &&.
          t ^. TripServiceId ==. c ^. CalendarServiceId &&.
          c ^. CalendarStartDate <=. val nowDate &&.
          c ^. CalendarEndDate >=. val nowDate &&.
          c ^. weekdaySqlExp
         )
  limit 3
  return s
  where earliest = timeToTimeOfDay $ timeOfDayToTime now - secondsToDiffTime 60
        latest = timeToTimeOfDay $ timeOfDayToTime now + secondsToDiffTime 60
        weekday = formatTime defaultTimeLocale "%A" nowDate
        weekdaySqlExp = weekdayToSQLExp weekday

runDBWithLogging dbName = runResourceT . runNoLoggingT . Sqlite.withSqliteConn dbName . Sqlite.runSqlConn
