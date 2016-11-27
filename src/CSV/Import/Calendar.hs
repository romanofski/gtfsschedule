{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module CSV.Import.Calendar where

import Data.Csv ( FromNamedRecord
                , DefaultOrdered
                , FromField
                , parseField
                )
import Control.Monad (mzero)
#if MIN_VERSION_time(1, 5, 0)
import Data.Time.Format (defaultTimeLocale)
#else
import System.Locale (defaultTimeLocale)
#endif
import Data.Time.Format ( parseTime)
import Data.Time.Calendar (Day)
import GHC.Generics

import Database.Persist (PersistValue(..))
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T


data Calendar = Calendar { service_id :: !T.Text
                         , monday :: !Int
                         , tuesday :: !Int
                         , wednesday :: !Int
                         , thursday :: !Int
                         , friday :: !Int
                         , saturday :: !Int
                         , sunday :: !Int
                         , start_date :: !Day
                         , end_date :: !Day
                         }
  deriving (Eq, Generic, Show)

instance FromNamedRecord Calendar
instance DefaultOrdered Calendar

-- 20160912
instance FromField Day where
  parseField str = case (parseTime defaultTimeLocale "%Y%m%d" (B.unpack str)) of
    Just d -> return d
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
                    , PersistDay $ start_date r
                    , PersistDay $ end_date r
                    ]
