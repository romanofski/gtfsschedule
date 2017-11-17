{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC  -fno-warn-orphans #-}
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
module TestProperty (proptests) where

import           Fixtures

import           GTFS.Realtime.Message.Internal                                       (makeVehicleInformation)
import           GTFS.Realtime.Message.Types                                          (departureTimeWithDelay)
import           GTFS.Schedule                                                        (ScheduleItem (..), ScheduleState (..), Stop (..), VehicleInformation (..), bumOffSeatTime, sortSchedules)

import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.FeedEntity        as FE
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.FeedHeader        as FH
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.FeedMessage       as FM
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TripDescriptor    as TD
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.VehicleDescriptor as VD
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.VehiclePosition   as VP

import           Data.Time.LocalTime                                                  (TimeOfDay (..))

import           Text.ProtocolBuffers.Basic                                           (uFromString)

import           Test.QuickCheck                                                      (Gen)
import           Test.Tasty                                                           (TestTree, testGroup)
import           Test.Tasty.QuickCheck                                                (Arbitrary (..), arbitraryBoundedEnum, choose, elements, listOf1, testProperty)

import           Control.Applicative                                                  (pure, (<$>), (<*>))


proptests :: TestTree
proptests =
    testGroup
        "property tests"
        [ testSortSchedules
        , testVehicleInformationIsPercentage]


testVehicleInformationIsPercentage :: TestTree
testVehicleInformationIsPercentage =
    testProperty
        "vehicle information is percentage"
        prop_VehicleInformationIsPercentage

prop_VehicleInformationIsPercentage :: VP.VehiclePosition -> Bool
prop_VehicleInformationIsPercentage vp = check $ makeVehicleInformation vp
  where check (VehicleInformation (Just x) (Just y)) = x <= 100 && x >= 0 && y >= 0 && y <= 100
        check (VehicleInformation (Just x) Nothing) = x <= 100 && x >= 0
        check (VehicleInformation Nothing (Just x)) = x <= 100 && x >= 0
        check (VehicleInformation Nothing Nothing) = True


testSortSchedules :: TestTree
testSortSchedules =
    testProperty
        "schedules are sorted by bum-off-seat-time"
        (\schedule -> propOrderedSchedule $ sortSchedules schedule)

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
        arbitrary <*>
        pure Nothing <*>
        pure testExtField <*>
        pure testUnknownField

instance Arbitrary VP.VehiclePosition where
    arbitrary =
        VP.VehiclePosition <$> arbitrary <*> arbitrary <*> pure Nothing <*>
        pure Nothing <*>
        pure Nothing <*>
        pure Nothing <*>
        pure Nothing <*>
        (Just <$> arbitraryBoundedEnum) <*>
        (Just <$> arbitraryBoundedEnum) <*>
        pure testExtField <*>
        pure testUnknownField

instance Arbitrary VD.VehicleDescriptor where
    arbitrary =
        VD.VehicleDescriptor <$> (Just . uFromString <$> listOf1 arbitrary) <*>
        (Just . uFromString <$> listOf1 arbitrary) <*>
        (Just . uFromString <$> listOf1 arbitrary) <*>
        pure testExtField <*>
        pure testUnknownField

instance Arbitrary TD.TripDescriptor where
    arbitrary =
        TD.TripDescriptor <$>
        (Just . uFromString <$> listOf1 arbitrary) <*>
        (Just . uFromString <$> arbitrary) <*>
        pure Nothing <*>
        pure Nothing <*>
        pure Nothing <*>
        (Just <$> arbitraryBoundedEnum) <*>
        pure testExtField <*>
        pure testUnknownField

arbitraryTimeOfDay :: Gen TimeOfDay
arbitraryTimeOfDay = TimeOfDay <$> choose (0, 23) <*> choose (0, 59) <*>
        (fromRational . toRational <$> choose (0 :: Double, 60))

arbitraryStop :: Gen Stop
arbitraryStop = Stop <$> arbitrary <*> arbitrary <*> arbitrary

arbitraryVehicleInformation :: Gen VehicleInformation
arbitraryVehicleInformation = VehicleInformation <$> arbitrary <*> arbitrary

instance Arbitrary ScheduleItem where
    arbitrary = do
        schedDepTime <- arbitraryTimeOfDay
        delay <- arbitrary
        trip <- listOf1 arbitrary
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
