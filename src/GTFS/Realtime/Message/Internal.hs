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
module GTFS.Realtime.Message.Internal where
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.VehiclePosition                 as VP
import           GTFS.Realtime.Internal.Com.Google.Transit.Realtime.VehiclePosition.CongestionLevel (CongestionLevel)
import           GTFS.Realtime.Internal.Com.Google.Transit.Realtime.VehiclePosition.OccupancyStatus (OccupancyStatus)
import           GTFS.Schedule                                                                      (VehicleInformation (..))

import qualified Text.ProtocolBuffers.Header                                                        as P'

makeVehicleInformation ::
  VP.VehiclePosition
  -> VehicleInformation
makeVehicleInformation vp = let congestionl = fromEnum (P'.getVal vp VP.congestion_level)
                                c_percentage = (congestionl * 100) `div` fromEnum (maxBound :: CongestionLevel)
                                occupancys = fromEnum (P'.getVal vp VP.occupancy_status)
                                o_percentage = (occupancys * 100) `div` fromEnum (maxBound :: OccupancyStatus)
                            in VehicleInformation (Just c_percentage) (Just o_percentage)

