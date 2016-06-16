module Realtime (feedTests) where

import Message
import Schedule

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase, (@?=))
import Data.Time.LocalTime (TimeOfDay(..))
import Data.Foldable (toList)
import Data.Maybe (catMaybes)
import Text.ProtocolBuffers (messageGet)

import qualified Com.Google.Transit.Realtime.FeedMessage as FM
import qualified Data.ByteString.Lazy as L


feedTests ::
  TestTree
feedTests = testGroup "realtime feed Tests"
            [ testSucessfullyUpdatesSchedule
            , testDepartureWithDelay
            ]

testSucessfullyUpdatesSchedule ::
  TestTree
testSucessfullyUpdatesSchedule = testCase "updates schedule from feed" $ do
  feed <- withFeed "test/data/feed.bin"
  let entities = getFeedEntities feed
  let tupdates = filterStopUpdates schedule $ filterTripUpdate schedule entities
  let items = catMaybes $ toList $ createScheduleItems schedule tupdates
  assertEqual "expecting updated delay" [expected] items
    where
      expected = ScheduleItem { tripId = "7136402-BT2015-04_FUL-Weekday-00"
                              , stopId = "10795"
                              , serviceName = "Test Service"
                              , scheduledDepartureTime = TimeOfDay 8 00 00
                              , departureDelay = -30
                              , departureTime = TimeOfDay 7 59 30
                              }
      schedule = [ ScheduleItem { tripId = "7136402-BT2015-04_FUL-Weekday-00"
                                , stopId = "10795"
                                , serviceName = "Test Service"
                                , scheduledDepartureTime = TimeOfDay 8 00 00
                                , departureDelay = 0
                                , departureTime = TimeOfDay 8 00 00
                                }]

testDepartureWithDelay ::
  TestTree
testDepartureWithDelay = testGroup "check departure with delay" $ makeTest <$>
  [ ("delayed", departureTimeWithDelay (TimeOfDay 7 0 0) 30, TimeOfDay 7 0 30)
  , ("ahead", departureTimeWithDelay (TimeOfDay 7 0 0) (-30), TimeOfDay 6 59 30)
  ]

withFeed ::
  FilePath
  -> IO FM.FeedMessage
withFeed fp = do
  contents <- L.readFile fp
  case messageGet contents of
    Left err -> error err
    Right (fm, _) -> return fm

makeTest (name, input, expected) = testCase name $ input @?= expected
