{-
Copyright (C) - 2017-2021 Róman Joost <roman@bromeco.de>

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
module GTFS.Realtime.Message.Types (ForFeedElement (..), departureTimeWithDelay) where

import Control.Lens (preview, to, view, _Just)
import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (secondsToDiffTime)
import Data.Time.LocalTime (TimeOfDay, timeToTimeOfDay)
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TripDescriptor as TD
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TripDescriptor.ScheduleRelationship as TripSR
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TripUpdate as TU
import GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TripUpdate.StopTimeEvent (delay)
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TripUpdate.StopTimeUpdate as STU
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TripUpdate.StopTimeUpdate.ScheduleRelationship as StopTUSR
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.VehiclePosition as VP
import GTFS.Realtime.Message.Internal (makeVehicleInformation)
import GTFS.Schedule (ScheduleItem (..), ScheduleState (..), Stop (..), secondsToDeparture)
import Text.ProtocolBuffers.Basic (uToString)
import qualified Text.ProtocolBuffers.Header as P'

class ForFeedElement e where
  getTripID :: e -> String
  getTripID x = fromMaybe "" $ preview (_Just . TD.trip_id . _Just . to uToString) (getTripDescriptor x)

  getTripDescriptor :: e -> Maybe TD.TripDescriptor
  updateScheduleItem :: e -> String -> ScheduleItem -> ScheduleItem

instance ForFeedElement TU.TripUpdate where
  getTripDescriptor x = Just $ view TU.trip x
  updateScheduleItem tu k item =
    if view (TU.trip . TD.schedule_relationship) tu == Just TripSR.CANCELED
      then
        ScheduleItem
          { tripId = k,
            stop = stop item,
            serviceName = serviceName item,
            scheduledDepartureTime = scheduledDepartureTime item,
            departureDelay = 0,
            departureTime = departureTime item,
            scheduleType = CANCELED,
            scheduleItemVehicleInformation = scheduleItemVehicleInformation item
          }
      else case findStopTimeUpdate (stop item) (getStopTimeUpdates tu) of
        Just stu ->
          ScheduleItem
            { tripId = k,
              stop = stop item,
              serviceName = serviceName item,
              scheduledDepartureTime = scheduledDepartureTime item,
              departureDelay = getDepartureDelay stu,
              departureTime =
                departureTimeWithDelay
                  (scheduledDepartureTime item)
                  (getDepartureDelay stu),
              scheduleType = scheduleTypeForStop stu,
              scheduleItemVehicleInformation = scheduleItemVehicleInformation item
            }
        Nothing -> item

instance ForFeedElement VP.VehiclePosition where
  getTripDescriptor x = view VP.trip x
  updateScheduleItem vp k item =
    if fromMaybe "" (preview (VP.stop_id . _Just . to uToString) vp)
      == stopIdentifier (stop item)
      then
        ScheduleItem
          { tripId = k,
            stop = stop item,
            serviceName = serviceName item,
            scheduledDepartureTime = scheduledDepartureTime item,
            departureDelay = departureDelay item,
            departureTime = departureTime item,
            scheduleType = scheduleType item,
            scheduleItemVehicleInformation = makeVehicleInformation vp
          }
      else item

getDepartureDelay ::
  STU.StopTimeUpdate ->
  Integer
getDepartureDelay update = fromMaybe 0 $ preview (STU.departure . _Just . delay . _Just . to fromIntegral) update

scheduleTypeForStop ::
  STU.StopTimeUpdate ->
  ScheduleState
scheduleTypeForStop update = if view STU.schedule_relationship update == Just StopTUSR.SKIPPED then CANCELED else SCHEDULED

-- | calculate the new departure time with a delay from the real time update
departureTimeWithDelay ::
  TimeOfDay ->
  Integer ->
  TimeOfDay
departureTimeWithDelay depTime d = timeToTimeOfDay $ secondsToDeparture depTime (secondsToDiffTime d)

getStopTimeUpdates ::
  TU.TripUpdate ->
  P'.Seq STU.StopTimeUpdate
getStopTimeUpdates = view TU.stop_time_update

findStopTimeUpdate ::
  Stop ->
  P'.Seq STU.StopTimeUpdate ->
  Maybe STU.StopTimeUpdate
findStopTimeUpdate s = find (\x -> stopTimeUpdateStopID x == stopIdentifier s)

stopTimeUpdateStopID ::
  STU.StopTimeUpdate ->
  String
stopTimeUpdateStopID = fromMaybe "" . preview (STU.stop_id . _Just . to uToString)
