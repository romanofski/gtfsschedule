module Main where

import Realtime (feedTests)

import Test.Tasty (defaultMain, TestTree, TestName, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Data.Time.LocalTime (TimeOfDay(..))
import Data.Time.Calendar (fromGregorian)
import Database.Persist (insert, entityVal)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad.Logger (NoLoggingT(..))
import qualified Data.Text as T

import qualified Database.Persist.Sqlite as Sqlite

import Schedule (ScheduleItem(..), minutesToDeparture, formatScheduleItem)
import qualified Database as DB

tests ::
  TestTree
tests = testGroup "unit tests" [ feedTests
                               , testMinutesToDeparture
                               , testFormatScheduleItem
                               , testDepartures
                               ]

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
                              }
      runningAhead = ScheduleItem { tripId = "."
                                  , stopId = "."
                                  , serviceName = "Running Ahead"
                                  , scheduledDepartureTime = TimeOfDay 7 50 00
                                  , departureDelay = -60
                                  , departureTime = TimeOfDay 7 49 00
                                  }
      runningLate = ScheduleItem { tripId = "."
                                  , stopId = "."
                                  , serviceName = "Running Late"
                                  , scheduledDepartureTime = TimeOfDay 7 50 00
                                  , departureDelay = 60
                                  , departureTime = TimeOfDay 7 51 00
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
                          }

makeTest ::
  (Eq a, Show a)
  => (TestName, a, a)
  -> TestTree
makeTest (name, input, expected) = testCase name $ input @?= expected

-- TODO: I'd like to make this a bit more generic, so that I can compare against
-- any accessors type
--
makeDatabaseTest (name, prepare, expected) = testCase name $ do
  stops <- DB.runDBWithoutLogging (T.pack ":memory:") prepare 
  let l = DB.stopTimeDepartureTime . entityVal . fst' <$> stops
  l @?= expected

testDepartures ::
  TestTree
testDepartures = testGroup "departure tests" $ makeDatabaseTest <$>
  [ ( "no departure because now is in future"
    , prepareStopTime >> DB.getNextDepartures "600029" (TimeOfDay 8 30 00) (fromGregorian 2015 1 7)
    , [])
  , ( "no departure because date is in future"
    , prepareStopTime >> DB.getNextDepartures "600029" (TimeOfDay 8 05 00) (fromGregorian 2015 2 7)
    , [])
  , ( "next departures are sorted"
    , prepareStopTime >> DB.getNextDepartures "600029" (TimeOfDay 8 05 00) (fromGregorian 2015 1 7)
    , [TimeOfDay 8 05 00, TimeOfDay 8 21 00])
  , ( "additional temp scheduled service"
    , prepareStopTime >> DB.getNextDepartures "600029" (TimeOfDay 8 05 00) (fromGregorian 2015 1 28)
    , [TimeOfDay 8 05 00, TimeOfDay 8 05 33, TimeOfDay 8 21 00])
  , ( "includes only temp scheduled service"
    , prepareStopTime >> DB.getNextDepartures "600029" (TimeOfDay 8 05 00) (fromGregorian 2015 2 4)
    , [TimeOfDay 8 05 33])
  ]


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
prepareStopTime = do
          _ <- Sqlite.runMigrationSilent DB.migrateAll
          let serviceId = "QF0815"
          routeId <- insert DB.Route { DB.routeShortName = "66"
                                     , DB.routeLongName = "Hell"
                                     , DB.routeDesc = Nothing
                                     , DB.routeType = "6"
                                     , DB.routeUrl = Nothing
                                     , DB.routeColor = Nothing
                                     , DB.routeTextColor = Nothing
                                     }
          tID <- insert DB.Trip { DB.tripRouteId = routeId
                                , DB.tripTripId = "QF0815-00"
                                , DB.tripServiceId = serviceId
                                , DB.tripHeadsign = Nothing
                                , DB.tripDirectionId = Nothing
                                , DB.tripShortName = Nothing
                                , DB.tripBlockId = Nothing
                                , DB.tripShapeId = Nothing
                                , DB.tripWheelchairAccessible = Nothing
                                , DB.tripBikesAllowed = Nothing
                                }

          t2 <- insert DB.Trip { DB.tripRouteId = routeId
                                , DB.tripTripId = "QF0815-00-Ekka"
                                , DB.tripServiceId = "ekka"
                                , DB.tripHeadsign = Nothing
                                , DB.tripDirectionId = Nothing
                                , DB.tripShortName = Nothing
                                , DB.tripBlockId = Nothing
                                , DB.tripShapeId = Nothing
                                , DB.tripWheelchairAccessible = Nothing
                                , DB.tripBikesAllowed = Nothing
                                }

          -- scheduled for only Wednesday
          _ <- insert DB.Calendar { DB.calendarServiceId = serviceId
                                  , DB.calendarMonday = False
                                  , DB.calendarTuesday = False
                                  , DB.calendarWednesday = True
                                  , DB.calendarThursday = False
                                  , DB.calendarFriday = False
                                  , DB.calendarSaturday = False
                                  , DB.calendarSunday = False
                                  , DB.calendarStartDate = fromGregorian 2011 1 1
                                  , DB.calendarEndDate = fromGregorian 2015 2 1
                                  }
          -- additional service running for a temporary time
          _ <- insert DB.Calendar { DB.calendarServiceId = "ekka"
                                  , DB.calendarMonday = False
                                  , DB.calendarTuesday = False
                                  , DB.calendarWednesday = True
                                  , DB.calendarThursday = False
                                  , DB.calendarFriday = False
                                  , DB.calendarSaturday = False
                                  , DB.calendarSunday = False
                                  , DB.calendarStartDate = fromGregorian 2015 1 25
                                  , DB.calendarEndDate = fromGregorian 2015 2 15
                                  }
          s1 <- insert DB.Stop { DB.stopStopId = "600029"
                                , DB.stopCode = Nothing
                                , DB.stopName = "."
                                , DB.stopDesc = Nothing
                                , DB.stopLat = 0.0
                                , DB.stopLon = 0.0
                                , DB.stopZoneId = Nothing
                                , DB.stopUrl = Nothing
                                , DB.stopLocationType = Nothing
                                , DB.stopParentStation = Nothing
                                }
          s2 <- insert DB.Stop { DB.stopStopId = "600019"
                                , DB.stopCode = Nothing
                                , DB.stopName = "."
                                , DB.stopDesc = Nothing
                                , DB.stopLat = 0.0
                                , DB.stopLon = 0.0
                                , DB.stopZoneId = Nothing
                                , DB.stopUrl = Nothing
                                , DB.stopLocationType = Nothing
                                , DB.stopParentStation = Nothing
                                }

          _ <- insert DB.StopTime { DB.stopTimeTripId = tID
                                  , DB.stopTimeTrip = "QF0815-00"
                                  , DB.stopTimeArrivalTime = TimeOfDay 8 02 00
                                  , DB.stopTimeDepartureTime = TimeOfDay 8 05 00
                                  , DB.stopTimeStop = "."
                                  , DB.stopTimeStopId = s1
                                  , DB.stopTimeStopSequence = "1"
                                  , DB.stopTimePickupType = Nothing
                                  , DB.stopTimeDropOffType = Nothing
                                  }

          _ <- insert DB.StopTime { DB.stopTimeTripId = t2
                                  , DB.stopTimeTrip = "QF-Service-Ekka"
                                  , DB.stopTimeArrivalTime = TimeOfDay 8 02 00
                                  , DB.stopTimeDepartureTime = TimeOfDay 8 05 33
                                  , DB.stopTimeStop = "."
                                  , DB.stopTimeStopId = s1
                                  , DB.stopTimeStopSequence = "1"
                                  , DB.stopTimePickupType = Nothing
                                  , DB.stopTimeDropOffType = Nothing
                                  }

          _ <- insert DB.StopTime { DB.stopTimeTripId = tID
                                  , DB.stopTimeTrip = "QF0819-00"
                                  , DB.stopTimeArrivalTime = TimeOfDay 8 02 00
                                  , DB.stopTimeDepartureTime = TimeOfDay 8 05 00
                                  , DB.stopTimeStop = "."
                                  , DB.stopTimeStopId = s2
                                  , DB.stopTimeStopSequence = "1"
                                  , DB.stopTimePickupType = Nothing
                                  , DB.stopTimeDropOffType = Nothing
                                  }

          insert DB.StopTime { DB.stopTimeTripId = tID
                             , DB.stopTimeTrip = "QF0815-00"
                             , DB.stopTimeArrivalTime = TimeOfDay 8 20 00
                             , DB.stopTimeDepartureTime = TimeOfDay 8 21 00
                             , DB.stopTimeStop = "."
                             , DB.stopTimeStopId = s1
                             , DB.stopTimeStopSequence = "1"
                             , DB.stopTimePickupType = Nothing
                             , DB.stopTimeDropOffType = Nothing
                             }

main ::
  IO ()
main = defaultMain tests
