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
import Control.Monad.Logger (LoggingT(..), runStderrLoggingT)
import Database.Persist.TH
import Database.Esqueleto
import Data.List (stripPrefix)
import qualified Data.Sequence as Sequence
import qualified Database.Persist.Sqlite as Sqlite

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
StopTime
    tripId TripId
    trip String
    arrivalTime TimeOfDay
    departureTime TimeOfDay
    stop String
    stopId StopId
    stopSequence String
    pickupType Int Maybe
    dropOffType Int Maybe
    deriving Show
Trip
    routeId String
    serviceId String
    tripId String
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
Stop
    stopId String
    code String Maybe
    name String
    desc String Maybe
    lat Double
    lon Double
    zoneId String Maybe
    url String Maybe
    locationType Int Maybe
    parentStation String Maybe
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

getStopID ::
  String
  -> ReaderT Sqlite.SqlBackend (NoLoggingT (ResourceT IO)) [Value String]
getStopID stopC = select $ from $ \s -> do
  where_ (s ^. StopCode ==. val (Just stopC))
  return (s ^. StopStopId)

getNextDepartures ::
  String
  -> TimeOfDay
  -> Day
  -> ReaderT Sqlite.SqlBackend (NoLoggingT (ResourceT IO)) [(Sqlite.Entity StopTime, Sqlite.Entity Trip)]
getNextDepartures stopID now nowDate = select $ from $ \(st, t, c, s) -> do
  where_ (
    st ^. StopTimeTripId ==. t ^. TripId &&.
      t ^. TripServiceId ==. c ^. CalendarServiceId &&.
      st ^. StopTimeStopId ==. s ^. StopId &&.
      s ^. StopStopId ==. val stopID &&.
      st ^. StopTimeDepartureTime >. val earliest &&.
      st ^. StopTimeDepartureTime <. val latest &&.
      c ^. weekdaySqlExp
    )
  orderBy [asc (st ^. StopTimeDepartureTime)]
  limit 3
  return (st, t)
  where earliest = timeToTimeOfDay $ timeOfDayToTime now - secondsToDiffTime 60
        latest = timeToTimeOfDay $ timeOfDayToTime now + secondsToDiffTime 60 * 30
        weekday = formatTime defaultTimeLocale "%A" nowDate
        weekdaySqlExp = weekdayToSQLExp weekday

runDBWithLogging dbName = runResourceT . runStderrLoggingT . Sqlite.withSqliteConn dbName . Sqlite.runSqlConn

runDBWithoutLogging dbName = runResourceT . runNoLoggingT . Sqlite.withSqliteConn dbName . Sqlite.runSqlConn
