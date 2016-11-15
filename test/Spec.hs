{-# LANGUAGE OverloadedStrings #-}
module Main where

import Schedule ( ScheduleItem(..)
                , ScheduleState(..)
                , TimeSpec(..)
                , minutesToDeparture
                , formatScheduleItem
                , getSchedule)
import qualified Database as DB
import qualified CSV.Import as CSV

import Realtime (feedTests)
import CSVImport (importTests)
import TestUpdate (updateTests)

import Data.Functor ((<$>))

import Test.Tasty (defaultMain, TestTree, TestName, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Data.Time.LocalTime (TimeOfDay(..))
import Data.Time.Calendar (fromGregorian)
import Database.Persist (insert)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad.Logger (NoLoggingT(..))
import System.IO.Temp (withSystemTempFile)
import System.Directory (getCurrentDirectory)

import qualified Database.Persist.Sqlite as Sqlite

tests ::
  TestTree
tests = testGroup "unit tests" [ feedTests
                               , importTests
                               , updateTests
                               , testMinutesToDeparture
                               , testFormatScheduleItem
                               , testDepartures
                               ]

makeTest ::
  (Eq a, Show a)
  => (TestName, a, a)
  -> TestTree
makeTest (name, input, expected) = testCase name $ input @?= expected

testFormatScheduleItem ::
  TestTree
testFormatScheduleItem = testGroup "formates schedule item" $ makeTest <$>
  [ ("punctual", formatScheduleItem (TimeOfDay 7 45 0) 0 punctual, "Punctual 5min (07:50:00) ")
  , ("punctual with walking delay", formatScheduleItem (TimeOfDay 7 45 0) 2 punctual, "Punctual 3min (07:50:00) ")
  , ("running late", formatScheduleItem (TimeOfDay 7 45 0) 0 runningLate, "!Running Late 6min (07:51:00 (60s)) ")
  , ("running late + walking delay", formatScheduleItem (TimeOfDay 7 45 0) 2 runningLate, "!Running Late 4min (07:51:00 (60s)) ")
  , ("running ahead", formatScheduleItem (TimeOfDay 7 45 0) 0 runningAhead, "Running Ahead 4min (07:49:00) ")
  , ("running ahead + walk delay", formatScheduleItem (TimeOfDay 7 45 0) 2 runningAhead, "Running Ahead 2min (07:49:00) ")
  ]
    where
      punctual = ScheduleItem { tripId = "."
                              , stopId = "."
                              , serviceName = "Punctual"
                              , scheduledDepartureTime = TimeOfDay 7 50 00
                              , departureDelay = 0
                              , departureTime = TimeOfDay 7 50 00
                              , scheduleType = SCHEDULED
                              }
      runningAhead = ScheduleItem { tripId = "."
                                  , stopId = "."
                                  , serviceName = "Running Ahead"
                                  , scheduledDepartureTime = TimeOfDay 7 50 00
                                  , departureDelay = -60
                                  , departureTime = TimeOfDay 7 49 00
                                  , scheduleType = SCHEDULED
                                  }
      runningLate = ScheduleItem { tripId = "."
                                  , stopId = "."
                                  , serviceName = "Running Late"
                                  , scheduledDepartureTime = TimeOfDay 7 50 00
                                  , departureDelay = 60
                                  , departureTime = TimeOfDay 7 51 00
                                  , scheduleType = SCHEDULED
                                  }

testMinutesToDeparture ::
  TestTree
testMinutesToDeparture = testGroup "calculates right delay" $ map makeTest
  [ ("simple", minutesToDeparture item (TimeOfDay 7 45 00), 6)
  , ("departure in past", minutesToDeparture item (TimeOfDay 7 52 00), -1)
  ]
    where
      item = ScheduleItem { tripId = "7136402-BT2015-04_FUL-Weekday-00"
                          , stopId = "10795"
                          , serviceName = "Test Service"
                          , scheduledDepartureTime = TimeOfDay 7 50 00
                          , departureDelay = 60
                          , departureTime = TimeOfDay 7 51 00
                          , scheduleType = SCHEDULED
                          }

makeDatabaseImportTest ::
  TestInput
  -> TestTree
makeDatabaseImportTest (TestInput name csvdatadir scode timespec expected) =
  testCase name $
  do withSystemTempFile
       "GTFSTest"
       (\tmpfile _ ->
          do
             cwd <- getCurrentDirectory
             CSV.runImport tmpfile $ concat [cwd, "/", "test", "/", "data", "/", csvdatadir]
             schedule <- getSchedule tmpfile scode timespec
             schedule @?= expected)


data TestInput = TestInput { testName :: String
                           , csvdatadirectory :: String
                           , stopcode :: String
                           , now :: TimeSpec
                           , testExpectedSchedule :: [ScheduleItem]
                           }

testDepartures ::
  TestTree
testDepartures =
    testGroup "departure tests with imports" $
    makeDatabaseImportTest <$>
    [ TestInput
      { testName = "no departure because date is past all scheduled services"
      , csvdatadirectory = "regular"
      , stopcode = "600029"
      , now = TimeSpec (TimeOfDay 8 5 0) (fromGregorian 2015 2 7)
      , testExpectedSchedule = []
      }
    , TestInput
      { testName = "no departure because time is past all scheduled services"
      , csvdatadirectory = "regular"
      , stopcode = "600029"
      , now = TimeSpec (TimeOfDay 8 5 0) (fromGregorian 2013 1 7)
      , testExpectedSchedule = []
      }
    , TestInput
      { testName = "imports aftermidnight services"
      , csvdatadirectory = "aftermidnight"
      , stopcode = "600029"
      , now = TimeSpec (TimeOfDay 1 0 0) (fromGregorian 2013 2 4)
      , testExpectedSchedule = [ ScheduleItem
                                 { tripId = "1"
                                 , stopId = "600029"
                                 , serviceName = "66 Graveyard Express"
                                 , scheduledDepartureTime = TimeOfDay 1 1 0
                                 , departureDelay = 0
                                 , departureTime = TimeOfDay 1 1 0
                                 , scheduleType = SCHEDULED
                                 }]
      }
    , TestInput
      { testName = "additional temp scheduled service"
      , csvdatadirectory = "tempservice"
      , stopcode = "600029"
      , now = TimeSpec (TimeOfDay 8 5 0) (fromGregorian 2015 1 28)
      , testExpectedSchedule = [ ScheduleItem
                                 { tripId = "QF0815-00"
                                 , stopId = "600029"
                                 , serviceName = "66 not relevant"
                                 , scheduledDepartureTime = TimeOfDay 8 5 0
                                 , departureDelay = 0
                                 , departureTime = TimeOfDay 8 5 0
                                 , scheduleType = SCHEDULED
                                 }
                               , ScheduleItem
                                 { tripId = "QF0815-00-Ekka"
                                 , stopId = "600029"
                                 , serviceName = "66 not relevant"
                                 , scheduledDepartureTime = TimeOfDay 8 5 33
                                 , departureDelay = 0
                                 , departureTime = TimeOfDay 8 5 33
                                 , scheduleType = SCHEDULED
                                 }
                               , ScheduleItem
                                 { tripId = "QF0815-00"
                                 , stopId = "600029"
                                 , serviceName = "66 not relevant"
                                 , scheduledDepartureTime = TimeOfDay 8 21 33
                                 , departureDelay = 0
                                 , departureTime = TimeOfDay 8 21 33
                                 , scheduleType = SCHEDULED
                                 }]
      }
    , TestInput
      { testName = "includes only temp scheduled service"
      , csvdatadirectory = "tempservice"
      , stopcode = "600029"
      , now = TimeSpec (TimeOfDay 8 5 0) (fromGregorian 2015 2 4)
      , testExpectedSchedule = [ ScheduleItem
                                 { tripId = "QF0815-00-Ekka"
                                 , stopId = "600029"
                                 , serviceName = "66 not relevant"
                                 , scheduledDepartureTime = TimeOfDay 8 5 33
                                 , departureDelay = 0
                                 , departureTime = TimeOfDay 8 5 33
                                 , scheduleType = SCHEDULED
                                 }]
      }]

fst' ::
  (a, b, c)
  -> a
fst' (x, _, _) = x

snd' ::
  (a, b, c)
  -> b
snd' (_, x, _) = x

-- | fixtures

prepareStopTime ::
  ReaderT Sqlite.SqlBackend (NoLoggingT (ResourceT IO)) (Sqlite.Key DB.StopTime)
prepareStopTime =
  do _ <- Sqlite.runMigrationSilent DB.migrateAll
     let serviceId = "QF0815"
     Sqlite.insert_
       DB.Route {DB.routeRouteId = "22-0815"
                ,DB.routeShortName = "66"
                ,DB.routeLongName = "Hell"
                ,DB.routeDesc = Nothing
                ,DB.routeType = "6"
                ,DB.routeUrl = Nothing
                ,DB.routeColor = Nothing
                ,DB.routeTextColor = Nothing}
     Sqlite.insert_
       DB.Trip {DB.tripRouteId = "22-0815"
               ,DB.tripTripId = "QF0815-00"
               ,DB.tripServiceId = serviceId
               ,DB.tripHeadsign = Nothing
               ,DB.tripDirectionId = Nothing
               ,DB.tripBlockId = Nothing
               ,DB.tripShapeId = Nothing}
     Sqlite.insert_
       DB.Trip {DB.tripRouteId = "22-0815"
               ,DB.tripTripId = "QF0815-00-Ekka"
               ,DB.tripServiceId = "ekka"
               ,DB.tripHeadsign = Nothing
               ,DB.tripDirectionId = Nothing
               ,DB.tripBlockId = Nothing
               ,DB.tripShapeId = Nothing}
     -- scheduled for only Wednesday
     Sqlite.insert_
       DB.Calendar {DB.calendarServiceId = serviceId
                   ,DB.calendarMonday = False
                   ,DB.calendarTuesday = False
                   ,DB.calendarWednesday = True
                   ,DB.calendarThursday = False
                   ,DB.calendarFriday = False
                   ,DB.calendarSaturday = False
                   ,DB.calendarSunday = False
                   ,DB.calendarStartDate =
                      fromGregorian 2011 1 1
                   ,DB.calendarEndDate =
                      fromGregorian 2015 2 1}
     -- additional service running for a temporary time
     Sqlite.insert_
       DB.Calendar {DB.calendarServiceId = "ekka"
                   ,DB.calendarMonday = False
                   ,DB.calendarTuesday = False
                   ,DB.calendarWednesday = True
                   ,DB.calendarThursday = False
                   ,DB.calendarFriday = False
                   ,DB.calendarSaturday = False
                   ,DB.calendarSunday = False
                   ,DB.calendarStartDate =
                      fromGregorian 2015 1 25
                   ,DB.calendarEndDate =
                      fromGregorian 2015 2 15}
     Sqlite.insert_
       DB.Stop {DB.stopStopId = "600029"
               ,DB.stopCode = Nothing
               ,DB.stopName = "."
               ,DB.stopDesc = Nothing
               ,DB.stopLat = 0.0
               ,DB.stopLon = 0.0
               ,DB.stopZoneId = Nothing
               ,DB.stopUrl = Nothing
               ,DB.stopLocationType = Nothing
               ,DB.stopParentStation = Nothing}
     Sqlite.insert_
       DB.Stop {DB.stopStopId = "600019"
               ,DB.stopCode = Nothing
               ,DB.stopName = "."
               ,DB.stopDesc = Nothing
               ,DB.stopLat = 0.0
               ,DB.stopLon = 0.0
               ,DB.stopZoneId = Nothing
               ,DB.stopUrl = Nothing
               ,DB.stopLocationType = Nothing
               ,DB.stopParentStation = Nothing}
     Sqlite.insert_
       DB.StopTime {DB.stopTimeTripId = "QF0815-00"
                   ,DB.stopTimeArrivalTime =
                      TimeOfDay 8 2 0
                   ,DB.stopTimeDepartureTime =
                      TimeOfDay 8 5 0
                   ,DB.stopTimeStopId = "600029"
                   ,DB.stopTimeStopSequence = 1
                   ,DB.stopTimePickupType = Nothing
                   ,DB.stopTimeDropOffType = Nothing}
     Sqlite.insert_
       DB.StopTime {DB.stopTimeTripId = "QF0815-00-Ekka"
                   ,DB.stopTimeArrivalTime =
                      TimeOfDay 8 2 0
                   ,DB.stopTimeDepartureTime =
                      TimeOfDay 8 5 33
                   ,DB.stopTimeStopId = "600029"
                   ,DB.stopTimeStopSequence = 1
                   ,DB.stopTimePickupType = Nothing
                   ,DB.stopTimeDropOffType = Nothing}
     Sqlite.insert_
       DB.StopTime {DB.stopTimeTripId = "QF0815-00"
                   ,DB.stopTimeArrivalTime =
                      TimeOfDay 8 2 0
                   ,DB.stopTimeDepartureTime =
                      TimeOfDay 8 5 0
                   ,DB.stopTimeStopId = "600019"
                   ,DB.stopTimeStopSequence = 1
                   ,DB.stopTimePickupType = Nothing
                   ,DB.stopTimeDropOffType = Nothing}
     insert DB.StopTime {DB.stopTimeTripId = "QF0815-00"
                        ,DB.stopTimeArrivalTime =
                           TimeOfDay 8 20 0
                        ,DB.stopTimeDepartureTime =
                           TimeOfDay 8 21 0
                        ,DB.stopTimeStopId = "600029"
                        ,DB.stopTimeStopSequence = 1
                        ,DB.stopTimePickupType = Nothing
                        ,DB.stopTimeDropOffType = Nothing}

main ::
  IO ()
main = defaultMain tests
