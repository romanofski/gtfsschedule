module TestProperty (proptests) where

import Fixtures

import GTFS.Realtime.Message.Schedule (updateSchedule, getTripUpdates)
import GTFS.Realtime.Message.Types (departureTimeWithDelay)
import GTFS.Schedule
       (ScheduleItem(..), ScheduleState(..),
        Stop(..), VehicleInformation(..),
        sortSchedules, bumOffSeatTime)

import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.FeedEntity as FE
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.FeedHeader as FH
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.FeedMessage as FM

import Data.Time.LocalTime (TimeOfDay(..))

import Text.ProtocolBuffers.Basic (uFromString)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
       (testProperty, elements, choose, Arbitrary(..), Gen)


proptests :: TestTree
proptests =
    testGroup
        "property tests"
        [testSortSchedules, testUpdateSchedulesKeepsLength]


testUpdateSchedulesKeepsLength :: TestTree
testUpdateSchedulesKeepsLength =
    testProperty
        "don't discard schedule items when updating from feed"
        (\schedule fm -> (length (updateSchedule (unwrapScheduleItems $ schedule) getTripUpdates fm) == length(schedule)))


testSortSchedules :: TestTree
testSortSchedules =
    testProperty
        "schedules are sorted by bum-off-seat-time"
        (\schedule -> propOrderedSchedule $ sortSchedules  $ unwrapScheduleItems schedule)

propOrderedSchedule :: [ScheduleItem] -> Bool
propOrderedSchedule [] = True
propOrderedSchedule [_] = True
propOrderedSchedule (x:y:rest) = (bumOffSeatTime x) <= (bumOffSeatTime y) && propOrderedSchedule rest



instance Arbitrary FM.FeedMessage where
    arbitrary =
        FM.FeedMessage <$> arbitrary <*> arbitrary <*> pure testExtField <*>
        pure testUnknownField

instance Arbitrary FH.FeedHeader where
    arbitrary =
        FH.FeedHeader <$> (uFromString <$> arbitrary) <*> pure Nothing <*> arbitrary <*>
        pure testExtField <*>
        pure testUnknownField

instance Arbitrary FE.FeedEntity where
    arbitrary =
        FE.FeedEntity <$> (uFromString <$> arbitrary) <*> arbitrary <*>
        pure Nothing <*>
        pure Nothing <*>
        pure Nothing <*>
        pure testExtField <*>
        pure testUnknownField

arbitraryTimeOfDay :: Gen TimeOfDay
arbitraryTimeOfDay = TimeOfDay <$> choose (0, 23) <*> choose (0, 59) <*>
        (fromRational . toRational <$> choose (0 :: Double, 60))

arbitraryStop :: Gen Stop
arbitraryStop = Stop <$> arbitrary <*> arbitrary <*> arbitrary

arbitraryVehicleInformation :: Gen VehicleInformation
arbitraryVehicleInformation = VehicleInformation <$> arbitrary <*> arbitrary

unwrapScheduleItems :: [ArbitraryScheduleItem] -> [ScheduleItem]
unwrapScheduleItems xs = unArbitrary <$> xs

-- | newtype declaration which wraps the schedule item to avoid orphaned
-- instances warning if we'd just implement the Arbitrary instance for
-- ScheduleItems here
--
newtype ArbitraryScheduleItem = ArbitraryScheduleItem
    { unArbitrary :: ScheduleItem
    } deriving (Show)

instance Arbitrary ArbitraryScheduleItem where
    arbitrary = ArbitraryScheduleItem <$> do
        schedDepTime <- arbitraryTimeOfDay
        delay <- arbitrary
        trip <- arbitrary
        s <- arbitraryStop
        name <- arbitrary
        stype <- elements [CANCELED, ADDED, SCHEDULED]
        vehicleInfo <- arbitraryVehicleInformation
        return $ ScheduleItem { tripId = trip
                              , stop = s
                              , serviceName = name
                              , scheduledDepartureTime = schedDepTime
                              , departureDelay = delay
                              , departureTime = departureTimeWithDelay schedDepTime delay
                              , scheduleType = stype
                              , scheduleItemVehicleInformation = vehicleInfo
                              }
