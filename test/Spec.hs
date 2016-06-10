module Main where

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
import Data.Time.LocalTime (TimeOfDay(..))
import Data.Time.Calendar (fromGregorian)
import Database.Persist (insert, entityVal)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad.Logger (NoLoggingT(..))
import Data.Foldable (toList)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Text.ProtocolBuffers (messageGet)

import qualified Com.Google.Transit.Realtime.FeedMessage as FM
import qualified Database.Persist.Sqlite as Sqlite
import qualified Data.ByteString.Lazy as L

import Message (departureTimeWithDelay, getFeedEntities, filterTripUpdate, filterStopUpdates, createScheduleItems, getDepartureDelay, getStopTimeUpdates)
import Schedule (ScheduleItem(..))
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
            , testDepartureWithDelay
            , testSucessfullyUpdatesSchedule
            ]

testSucessfullyUpdatesSchedule ::
  TestTree
testSucessfullyUpdatesSchedule = testCase "updates schedule from feed" $ do
  feed <- withFeed "test/data/feed.bin"
  let entities = getFeedEntities feed
  let tupdates = filterStopUpdates scheduleItemFixture $ filterTripUpdate scheduleItemFixture entities
  let items = catMaybes $ toList $ createScheduleItems scheduleItemFixture tupdates
  assertEqual "expecting updated delay" [expected] items
    where
      expected = ScheduleItem { tripId = "7136402-BT2015-04_FUL-Weekday-00"
                              , stopId = "10795"
                              , serviceName = "Test Service"
                              , scheduledDepartureTime = TimeOfDay 8 00 00
                              , departureDelay = -30
                              , departureTime = TimeOfDay 7 59 30
                              }


scheduleItemFixture ::
  [ScheduleItem]
scheduleItemFixture =
  [ ScheduleItem { tripId = "7136402-BT2015-04_FUL-Weekday-00"
                 , stopId = "10795"
                 , serviceName = "Test Service"
                 , scheduledDepartureTime = TimeOfDay 8 00 00
                 , departureDelay = 0
                 , departureTime = TimeOfDay 8 00 00
                 }
  ]

testDepartureWithDelay ::
  TestTree
testDepartureWithDelay = testCase "check departure with delay" $
  assertEqual "expected delayed departure" (TimeOfDay 7 0 30) (departureTimeWithDelay (TimeOfDay 7 0 0) 30)

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

withFeed ::
  FilePath
  -> IO FM.FeedMessage
withFeed fp = do
  contents <- L.readFile fp
  case messageGet contents of
    Left err -> error err
    Right (fm, _) -> return fm

main ::
  IO ()
main = defaultMain tests
