{-# LANGUAGE DeriveGeneric #-}
module CSV.Calendar where

import qualified Database as DB

import Data.Csv ( FromNamedRecord
                , DefaultOrdered
                , FromField
                , parseField
                )
import Data.Time.Format ( parseTimeM
                        , defaultTimeLocale)
import Data.Time.Calendar (Day)
import GHC.Generics

import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.Trans.Resource (MonadResource)
import Database.Esqueleto
import qualified Database.Persist.Sqlite as Sqlite
import qualified Data.ByteString.Char8 as B


data Calendar = Calendar { service_id :: !String
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
  parseField str = parseTimeM False defaultTimeLocale "%Y%m%d" (B.unpack str)

toBool ::
  Int
  -> Bool
toBool 0 = False
toBool _ = True

insertIntoDB ::
  (MonadLoggerIO m, MonadResource m)
  => Calendar
  -> ReaderT Sqlite.SqlBackend m (Key DB.Calendar)
insertIntoDB r = insert $ DB.Calendar { DB.calendarServiceId = service_id r
                                      , DB.calendarMonday = toBool $ monday r
                                      , DB.calendarTuesday = toBool $ tuesday r
                                      , DB.calendarWednesday = toBool $ wednesday r
                                      , DB.calendarThursday = toBool $ thursday r
                                      , DB.calendarFriday = toBool $ friday r
                                      , DB.calendarSaturday = toBool $ saturday r
                                      , DB.calendarSunday = toBool $ sunday r
                                      , DB.calendarStartDate = start_date r
                                      , DB.calendarEndDate = end_date r
                                      }
