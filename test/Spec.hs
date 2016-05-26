module Main where

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
import Data.Time.LocalTime (TimeOfDay(..))
import Data.Time.Calendar (fromGregorian)
import Database.Persist (insert, entityVal)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad.Logger (NoLoggingT(..))
import qualified Data.Text as T

import qualified Database.Persist.Sqlite as Sqlite

import qualified Database as DB

tests ::
  TestTree
tests = testGroup "unit tests" [unitTests]

unitTests ::
  TestTree
unitTests = testGroup "schedule tests"
            [ testGetsNextDepartures
            , testNextDeparturesAreSorted
            , testNoDeparturesForFuturetime
            ]

testGetsNextDepartures ::
  TestTree
testGetsNextDepartures = testCase "check next departures" $ do
  stops <- DB.runDBWithoutLogging (T.pack ":memory:") $ prepareStopTime >> nextDeparturesFromNow
  let l = length $ entityVal . snd <$> stops
  assertEqual "expected one stop time" 2 l

testNextDeparturesAreSorted ::
  TestTree
testNextDeparturesAreSorted = testCase "next departures are sorted" $ do
  stops <- DB.runDBWithoutLogging (T.pack ":memory:") $ prepareStopTime >> nextDeparturesFromNow
  let l = DB.stopTimeDepartureTime . entityVal . fst <$> stops
  assertEqual "expected one stop time" [TimeOfDay 8 05 00, TimeOfDay 8 21 00] l

testNoDeparturesForFuturetime ::
  TestTree
testNoDeparturesForFuturetime = testCase "no departures with future time" $ do
  stops <- DB.runDBWithoutLogging (T.pack ":memory:") $ prepareStopTime >> nextDeparturesFuture
  assertBool "expected no departure" (null $ entityVal . fst <$> stops)
  where nextDeparturesFuture = DB.getNextDepartures "600029" (TimeOfDay 8 30 00) (fromGregorian 2015 1 7)

nextDeparturesFromNow ::
  ReaderT Sqlite.SqlBackend (NoLoggingT (ResourceT IO)) [(Sqlite.Entity DB.StopTime, Sqlite.Entity DB.Trip)]
nextDeparturesFromNow = DB.getNextDepartures "600029" (TimeOfDay 8 05 00) (fromGregorian 2015 1 7)

prepareStopTime ::
  ReaderT Sqlite.SqlBackend (NoLoggingT (ResourceT IO)) (Sqlite.Key DB.StopTime)
prepareStopTime = do
          _ <- Sqlite.runMigrationSilent DB.migrateAll
          let serviceId = "QF0815"
          tID <- insert DB.Trip { DB.tripRouteId = "."
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
                                  , DB.calendarEndDate = fromGregorian 2020 1 1
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
