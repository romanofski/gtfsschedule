{-# LANGUAGE OverloadedStrings #-}
-- | A real time update from the GTFS feed
module Message where

import Com.Google.Transit.Realtime.TripUpdate.StopTimeEvent (StopTimeEvent(..))
import Com.Google.Transit.Realtime.TripDescriptor (trip_id)
import qualified Com.Google.Transit.Realtime.TripUpdate.StopTimeUpdate as STU
import qualified Com.Google.Transit.Realtime.FeedMessage as FM
import qualified Com.Google.Transit.Realtime.TripUpdate as TU
import qualified Com.Google.Transit.Realtime.FeedEntity as FE

import qualified Database.Persist.Sqlite as Sqlite

import Text.ProtocolBuffers (utf8)
import Text.ProtocolBuffers.Basic (Utf8)
import qualified Text.ProtocolBuffers.Header as P'
import qualified Data.ByteString.Lazy.UTF8 as U (toString)
import Data.Time.LocalTime (LocalTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Clock (UTCTime)
import Data.Foldable (toList)
import Control.Monad (mfilter)
import qualified Data.Set as Set

import Database ( Trip
                , tripTripId
                , StopTime
                , stopTimeStop
                )

getFeedEntities ::
  FM.FeedMessage
  -> P'.Seq TU.TripUpdate
getFeedEntities fm = (`P'.getVal` FE.trip_update) <$> entity
  where entity = P'.getVal fm FM.entity

filterTripUpdate ::
  [Sqlite.Entity Trip]
  -> P'.Seq TU.TripUpdate
  -> P'.Seq TU.TripUpdate
filterTripUpdate xs = mfilter (\x -> getTripID x `elem` relevantTripIDs)
  where
    relevantTripIDs = tripTripId . Sqlite.entityVal <$> xs

getTripID ::
  TU.TripUpdate
  -> String
getTripID x = utf8ToString tripId
  where
    descriptor = P'.getVal x TU.trip
    tripId = P'.getVal descriptor trip_id

filterStopUpdates ::
  [Sqlite.Entity StopTime]
  -> P'.Seq TU.TripUpdate
  -> P'.Seq TU.TripUpdate
filterStopUpdates xs = mfilter (Set.isSubsetOf relevantStopIDs . getStopTimeUpdateStopIDs)
  where
    -- TODO meh perhaps this can be done better instead of going from Seq -> List -> Set?
    relevantStopIDs = Set.fromList $ stopTimeStop . Sqlite.entityVal <$> xs

getStopTimeUpdateStopIDs ::
  TU.TripUpdate
  -> Set.Set String
getStopTimeUpdateStopIDs x = Set.fromList $ utf8ToString <$> utf8StopIds
  where
    stopTimeUpdates = P'.getVal x TU.stop_time_update
    utf8StopIds = toList $ (`P'.getVal` STU.stop_id) <$> stopTimeUpdates

getDepartureTime ::
  StopTimeEvent
  -> UTCTime
getDepartureTime event = depTime
  where d = toInteger $ P'.getVal event delay
        t = toInteger $ P'.getVal event time
        depTime = secondsToTime $ d + t

-- | format (e.g. departure) time to use local time
-- Note: Uses defaultTimeLocale
--
formatUTCTime ::
  LocalTime
  -> String
formatUTCTime = formatTime defaultTimeLocale "%R"


-- private helpers
--
utf8ToString ::
  Utf8
  -> String
utf8ToString = U.toString . utf8

-- | convert our unix epoch time to a real time format so we can use it to display departure time
--
secondsToTime ::
  Integral a =>
  a
  -> UTCTime
secondsToTime x = posixSecondsToUTCTime $ realToFrac x
