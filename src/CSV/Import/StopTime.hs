{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
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
module CSV.Import.StopTime where

import           CSV.Import.Util      (maybeToPersist)

import           Data.Csv             (DefaultOrdered, FromNamedRecord, FromField(..))
import           GHC.Generics         hiding (from)
import           Data.Time.LocalTime  (TimeOfDay (..))
import           Data.Time.Format     (parseTimeM, defaultTimeLocale)

import           Data.Int             (Int64)
import qualified Data.Text            as T
import           Database.Persist     (PersistValue (..))
import           Control.Monad         (mzero)
import qualified Data.ByteString.Char8 as B

data StopTime = StopTime { trip_id        :: !T.Text
                         , arrival_time   :: WrappedTimeOfDay
                         , departure_time :: WrappedTimeOfDay
                         , stop_id        :: !T.Text
                         , stop_sequence  :: !Int64
                         , pickup_type    :: Maybe Int64
                         , drop_off_type  :: Maybe Int64
                         }
  deriving (Eq, Generic, Show)

instance FromNamedRecord StopTime
instance DefaultOrdered StopTime

newtype WrappedTimeOfDay = WrappedTimeOfDay { unWrap :: TimeOfDay } deriving (Eq, Show)

instance FromField WrappedTimeOfDay where
  parseField str = case parseTimeM True defaultTimeLocale "%T" (fixUpTimes $ B.unpack str) of
    Just d -> return $ WrappedTimeOfDay d
    Nothing -> mzero

-- | Fix times which indicate +1 day by simply resetting them to early morning times.
--
-- Perhaps we'll have to invent a new data type in SQL
-- just to be able to correctly query those times :/
fixUpTimes ::
  String
  -> String
fixUpTimes = go
  where go ('2':'5':rest) = "01" ++ rest
        go ('2':'6':rest) = "02" ++ rest
        go ('2':'7':rest) = "03" ++ rest
        go ('2':'8':rest) = "04" ++ rest
        go ('2':'9':rest) = "05" ++ rest
        go ('3':'0':rest) = "06" ++ rest
        go ('3':'1':rest) = "07" ++ rest
        go xs = xs

prepareSQL ::
  T.Text
prepareSQL = "insert into stop_time (trip_id, arrival_time, departure_time, stop_id, stop_sequence, pickup_type, drop_off_type) \
            \ values (?, ?, ?, ?, ?, ?, ?)"

convertToValues ::
  StopTime
  -> [PersistValue]
convertToValues st = [ PersistText $ trip_id st
                     , PersistTimeOfDay $ unWrap $ arrival_time st
                     , PersistTimeOfDay $ unWrap $ departure_time st
                     , PersistText $ stop_id st
                     , PersistInt64 $ stop_sequence st
                     , maybeToPersist PersistInt64 (pickup_type st)
                     , maybeToPersist PersistInt64 (drop_off_type st)
                     ]
