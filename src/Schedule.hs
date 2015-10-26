-- | the GTFS schedule
module Schedule
    ( lookupStopInfo
    ) where

import Data.GTFS.Parse (parseFile)

-- | returns stop name from given stopid
-- TODO: currently hard coded to roma street platform 8 (600029)
--
lookupStopName ::
  String
  -> String
lookupStopName stopId = filter
  where stopTimes = parseFile "stop_times.txt"
