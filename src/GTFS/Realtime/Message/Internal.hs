module GTFS.Realtime.Message.Internal where

import GTFS.Schedule
       (VehicleInformation(..))
import qualified
       GTFS.Realtime.Internal.Com.Google.Transit.Realtime.VehiclePosition
       as VP
import GTFS.Realtime.Internal.Com.Google.Transit.Realtime.VehiclePosition.CongestionLevel
       (CongestionLevel)
import GTFS.Realtime.Internal.Com.Google.Transit.Realtime.VehiclePosition.OccupancyStatus
       (OccupancyStatus)

import qualified Text.ProtocolBuffers.Header as P'

makeVehicleInformation ::
  VP.VehiclePosition
  -> VehicleInformation
makeVehicleInformation vp = let congestionl = fromEnum (P'.getVal vp VP.congestion_level)
                                c_percentage = (congestionl * 100) `div` fromEnum (maxBound :: CongestionLevel)
                                occupancys = fromEnum (P'.getVal vp VP.occupancy_status)
                                o_percentage = (occupancys * 100) `div` fromEnum (maxBound :: OccupancyStatus)
                            in VehicleInformation (Just c_percentage) (Just o_percentage)

