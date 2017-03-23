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
module GTFS.Realtime.Message.Types (ForFeedElement(..), departureTimeWithDelay) where

import           Data.Foldable                                                                                     (find)
import           Data.Time.Clock                                                                                   (secondsToDiffTime)
import           Data.Time.LocalTime                                                                               (TimeOfDay, timeToTimeOfDay)
import           GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TripDescriptor                                 (TripDescriptor (..), trip_id)
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TripDescriptor.ScheduleRelationship            as TripSR
import           GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TripUpdate                                     (TripUpdate (..))
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TripUpdate.StopTimeEvent                       (delay)
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TripUpdate.StopTimeUpdate                      as STU
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TripUpdate.StopTimeUpdate.ScheduleRelationship as StopTUSR
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.VehiclePosition                                as VP
import           GTFS.Realtime.Message.Internal                                                                    (makeVehicleInformation)
import           GTFS.Schedule                                                                                     (ScheduleItem (..), ScheduleState (..), Stop (..), secondsToDeparture)
import           Text.ProtocolBuffers.Basic                                                                        (uToString)
import qualified Text.ProtocolBuffers.Header                                                                       as P'

class ForFeedElement e where
  getTripID :: e -> String
  getTripID x = uToString $ P'.getVal (getTripDescriptor x) trip_id

  getTripDescriptor :: e -> TripDescriptor
  updateScheduleItem :: e -> String -> ScheduleItem -> ScheduleItem


instance ForFeedElement TripUpdate where
    getTripDescriptor x = P'.getVal x trip
    updateScheduleItem TripUpdate{trip = TripDescriptor{schedule_relationship = Just TripSR.CANCELED}} k item = ScheduleItem
            { tripId = k
            , stop = stop item
            , serviceName = serviceName item
            , scheduledDepartureTime = scheduledDepartureTime item
            , departureDelay = 0
            , departureTime = departureTime item
            , scheduleType = CANCELED
            , scheduleItemVehicleInformation = scheduleItemVehicleInformation item
            }
    updateScheduleItem tu k item =
        case (findStopTimeUpdate (stop item) (getStopTimeUpdates tu)) of
          Just stu -> ScheduleItem
            { tripId = k
            , stop = stop item
            , serviceName = serviceName item
            , scheduledDepartureTime = scheduledDepartureTime item
            , departureDelay = getDepartureDelay stu
            , departureTime = departureTimeWithDelay
                  (scheduledDepartureTime item)
                  (getDepartureDelay stu)
            , scheduleType = scheduleTypeForStop stu
            , scheduleItemVehicleInformation = scheduleItemVehicleInformation item
            }
          Nothing -> item

instance ForFeedElement VP.VehiclePosition where
    getTripDescriptor x = P'.getVal x VP.trip
    updateScheduleItem vp k item =
        if (uToString $ P'.getVal vp VP.stop_id) ==
           (stopIdentifier $ stop item)
            then ScheduleItem
                 { tripId = k
                 , stop = stop item
                 , serviceName = serviceName item
                 , scheduledDepartureTime = scheduledDepartureTime item
                 , departureDelay = departureDelay item
                 , departureTime = departureTime item
                 , scheduleType = scheduleType item
                 , scheduleItemVehicleInformation = makeVehicleInformation vp
                 }
            else item

getDepartureDelay ::
  STU.StopTimeUpdate
  -> Integer
getDepartureDelay update = fromIntegral $ P'.getVal d GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TripUpdate.StopTimeEvent.delay
  where d = P'.getVal update STU.departure

scheduleTypeForStop ::
  STU.StopTimeUpdate
  -> ScheduleState
scheduleTypeForStop STU.StopTimeUpdate { STU.schedule_relationship = Just StopTUSR.SKIPPED } = CANCELED
scheduleTypeForStop _ = SCHEDULED

-- | calculate the new departure time with a delay from the real time update
departureTimeWithDelay ::
  TimeOfDay
  -> Integer
  -> TimeOfDay
departureTimeWithDelay depTime d = timeToTimeOfDay $ secondsToDeparture depTime (secondsToDiffTime d)

getStopTimeUpdates ::
  TripUpdate
  -> P'.Seq STU.StopTimeUpdate
getStopTimeUpdates msg = P'.getVal msg stop_time_update

findStopTimeUpdate ::
  Stop
  -> P'.Seq STU.StopTimeUpdate
  -> Maybe STU.StopTimeUpdate
findStopTimeUpdate s = find (\x -> stopTimeUpdateStopID x == stopIdentifier s)

stopTimeUpdateStopID ::
  STU.StopTimeUpdate
  -> String
stopTimeUpdateStopID msg = uToString $ P'.getVal msg STU.stop_id
