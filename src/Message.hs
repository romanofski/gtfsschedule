{-# LANGUAGE OverloadedStrings #-}
-- | A real time update from the GTFS feed
module Message where

import Schedule (ScheduleItem(..), secondsToDeparture, printSchedule)

import Com.Google.Transit.Realtime.TripUpdate.StopTimeEvent (StopTimeEvent(..), delay)
import Com.Google.Transit.Realtime.TripDescriptor (trip_id)
import qualified Com.Google.Transit.Realtime.TripUpdate.StopTimeUpdate as STU
import qualified Com.Google.Transit.Realtime.FeedMessage as FM
import qualified Com.Google.Transit.Realtime.TripUpdate as TU
import qualified Com.Google.Transit.Realtime.FeedEntity as FE

import Text.ProtocolBuffers (utf8)
import Text.ProtocolBuffers.Basic (Utf8)
import qualified Text.ProtocolBuffers.Header as P'
import qualified Data.ByteString.Lazy.UTF8 as U (toString)
import Data.Time.LocalTime (timeToTimeOfDay, TimeOfDay)
import Data.Time.Clock (secondsToDiffTime)
import Data.Foldable (toList, find)
import Control.Monad (mfilter)
import Data.Maybe (catMaybes)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set


printUpdatedSchedule ::
  FM.FeedMessage
  -> Integer
  -> [ScheduleItem]
  -> IO ()
printUpdatedSchedule fm walkDelay schedule =
  if Seq.null tupdates
  then printSchedule walkDelay schedule
       -- Yikes!! TODO
  else printSchedule walkDelay $ catMaybes $ toList $ createScheduleItems schedule tupdates
  where
      entities = getFeedEntities fm
      tupdates = filterStopUpdates schedule $ filterTripUpdate schedule entities

getFeedEntities ::
  FM.FeedMessage
  -> P'.Seq TU.TripUpdate
getFeedEntities fm = (`P'.getVal` FE.trip_update) <$> entity
  where entity = P'.getVal fm FM.entity

filterTripUpdate ::
  [ScheduleItem]
  -> P'.Seq TU.TripUpdate
  -> P'.Seq TU.TripUpdate
filterTripUpdate xs = mfilter (\x -> getTripID x `elem` relevantTripIDs)
  where
    relevantTripIDs = tripId <$> xs

getTripID ::
  TU.TripUpdate
  -> String
getTripID x = utf8ToString tripID
  where
    descriptor = P'.getVal x TU.trip
    tripID = P'.getVal descriptor trip_id

filterStopUpdates ::
  [ScheduleItem]
  -> P'.Seq TU.TripUpdate
  -> P'.Seq TU.TripUpdate
filterStopUpdates xs = mfilter (Set.isSubsetOf relevantStopIDs . getStopTimeUpdateStopIDs)
  where
    -- TODO meh perhaps this can be done better instead of going from Seq -> List -> Set?
    relevantStopIDs = Set.fromList $ stopId <$> xs

getStopTimeUpdateStopIDs ::
  TU.TripUpdate
  -> Set.Set String
getStopTimeUpdateStopIDs x = Set.fromList $ utf8ToString <$> utf8StopIds
  where
    stopTimeUpdates = P'.getVal x TU.stop_time_update
    utf8StopIds = toList $ (`P'.getVal` STU.stop_id) <$> stopTimeUpdates

createScheduleItem ::
  ScheduleItem
  -> Maybe STU.StopTimeUpdate
  -> Maybe ScheduleItem
createScheduleItem _ Nothing = Nothing
createScheduleItem item (Just stu) = Just
  ScheduleItem { tripId = tripId item
               , stopId = stopId item
               , serviceName = serviceName item
               , scheduledDepartureTime = scheduledDepartureTime item
               , departureDelay = getDepartureDelay stu
               , departureTime = departureTimeWithDelay (scheduledDepartureTime item) (getDepartureDelay stu)
               }

departureTimeWithDelay ::
  TimeOfDay
  -> Integer
  -> TimeOfDay
departureTimeWithDelay depTime d = timeToTimeOfDay $ secondsToDeparture depTime (secondsToDiffTime d)

createScheduleItems ::
  [ScheduleItem]
  -> P'.Seq TU.TripUpdate
  -> P'.Seq (Maybe ScheduleItem)
createScheduleItems items tupdates = ( \x -> createScheduleItem x (maybeSTU x $ maybeTU x) ) <$> Seq.fromList items
  where
    maybeTU s = findTripUpdate (tripId s) tupdates
    maybeSTU _ Nothing = error "This shouldn't happen"
    maybeSTU s (Just z) = findStopTimeUpdate (stopId s) (getStopTimeUpdates z)

getStopTimeUpdates ::
  TU.TripUpdate
  -> P'.Seq STU.StopTimeUpdate
getStopTimeUpdates msg = P'.getVal msg TU.stop_time_update

findTripUpdate ::
  String
  -> P'.Seq TU.TripUpdate
  -> Maybe TU.TripUpdate
findTripUpdate tripID = find (\x -> getTripID x == tripID)

findStopTimeUpdate ::
  String
  -> P'.Seq STU.StopTimeUpdate
  -> Maybe STU.StopTimeUpdate
findStopTimeUpdate stopID = find (\x -> stopTimeUpdateStopID x == stopID)

stopTimeUpdateStopID ::
  STU.StopTimeUpdate
  -> String
stopTimeUpdateStopID msg = utf8ToString $ P'.getVal msg STU.stop_id

getDepartureDelay ::
  STU.StopTimeUpdate
  -> Integer
getDepartureDelay update = fromIntegral $ P'.getVal d delay
  where d = P'.getVal update STU.departure

-- private helpers
--
utf8ToString ::
  Utf8
  -> String
utf8ToString = U.toString . utf8
