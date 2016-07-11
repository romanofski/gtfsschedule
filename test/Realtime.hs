module Realtime (feedTests) where

import Message
import Schedule

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase, (@?=))
import Data.Time.LocalTime (TimeOfDay(..))
import Text.ProtocolBuffers (messageGet)

import qualified Com.Google.Transit.Realtime.FeedMessage as FM
import qualified Data.ByteString.Lazy as L


feedTests ::
  TestTree
feedTests = testGroup "realtime feed Tests"
            [ testDepartureWithDelay
            , testUpdatesScheduleInOrder
            ]

testUpdatesScheduleInOrder ::
  TestTree
testUpdatesScheduleInOrder = testCase "updates in order" $ do
  feed <- withFeed "test/data/feed.bin"
  let items =  updateSchedule schedule feed
  assertEqual "expecting updated delay" expected items
    where
      expected = [ ScheduleItem { tripId = "7241124-BCC2015-BCC_FUL-M-Tu-W-Th-10"
                                , stopId = "317579"
                                , serviceName = "S2"
                                , scheduledDepartureTime = TimeOfDay 10 30 00
                                , departureDelay = -179
                                , departureTime = TimeOfDay 10 27 01
                                }
                 , ScheduleItem { tripId = "7136402-BT2015-04_FUL-Weekday-00"
                                , stopId = "10795"
                                , serviceName = "S1"
                                , scheduledDepartureTime = TimeOfDay 8 00 00
                                , departureDelay = -30
                                , departureTime = TimeOfDay 7 59 30
                                }
                 ]
      schedule = [ ScheduleItem { tripId = "7241124-BCC2015-BCC_FUL-M-Tu-W-Th-10"
                                , stopId = "317579"
                                , serviceName = "S2"
                                , scheduledDepartureTime = TimeOfDay 10 30 00
                                , departureDelay = 0
                                , departureTime = TimeOfDay 10 30 00
                                }
                 , ScheduleItem { tripId = "7136402-BT2015-04_FUL-Weekday-00"
                                , stopId = "10795"
                                , serviceName = "S1"
                                , scheduledDepartureTime = TimeOfDay 8 00 00
                                , departureDelay = 0
                                , departureTime = TimeOfDay 8 00 00
                                }
                 ]

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
