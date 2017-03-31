{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
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
module CSV.Import.Calendar where

import           Control.Monad         (mzero)
import           Data.Csv              (DefaultOrdered, FromField,
                                        FromNamedRecord(..), parseField, (.:))
#if MIN_VERSION_time(1, 5, 0)
import           Data.Time.Format      (defaultTimeLocale)
#else
import           System.Locale         (defaultTimeLocale)
#endif
import           Data.Time.Calendar    (Day)
import           Data.Time.Format      (parseTime)
import           GHC.Generics

import qualified Data.ByteString.Char8 as B
import qualified Data.Text             as T
import           Database.Persist      (PersistValue (..))


data Calendar = Calendar { service_id :: !T.Text
                         , monday     :: !Int
                         , tuesday    :: !Int
                         , wednesday  :: !Int
                         , thursday   :: !Int
                         , friday     :: !Int
                         , saturday   :: !Int
                         , sunday     :: !Int
                         , start_date :: CSVDay
                         , end_date   :: CSVDay
                         }
  deriving (Eq, Generic, Show)

instance FromNamedRecord Calendar where
    parseNamedRecord m =
        Calendar <$> m .: "service_id" <*> m .: "monday" <*> m .: "tuesday" <*>
        m .: "wednesday" <*>
        m .: "thursday" <*>
        m .: "friday" <*>
        m .: "saturday" <*>
        m .: "sunday" <*>
        m .: "start_date" <*>
        m .: "end_date"

instance DefaultOrdered Calendar

-- Wrapper to avoid orphaned instances (see #29)
-- Format: 20160912
newtype CSVDay = CSVDay { unWrapDay :: Day } deriving (Eq, Show)

instance FromField CSVDay where
  parseField str = case (parseTime defaultTimeLocale "%Y%m%d" (B.unpack str)) of
    Just d -> return $ CSVDay d
    Nothing -> mzero

toBool ::
  Int
  -> Bool
toBool 0 = False
toBool _ = True

prepareSQL ::
  T.Text
prepareSQL = "insert into calendar (service_id, monday, tuesday, wednesday, thursday, friday, saturday, sunday, start_date, end_date) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

convertToValues ::
  Calendar
  -> [PersistValue]
convertToValues r = [ PersistText $ service_id r
                    , PersistBool $ toBool $ monday r
                    , PersistBool $ toBool $ tuesday r
                    , PersistBool $ toBool $ wednesday r
                    , PersistBool $ toBool $ thursday r
                    , PersistBool $ toBool $ friday r
                    , PersistBool $ toBool $ saturday r
                    , PersistBool $ toBool $ sunday r
                    , PersistDay $ unWrapDay $ start_date r
                    , PersistDay $ unWrapDay $ end_date r
                    ]
