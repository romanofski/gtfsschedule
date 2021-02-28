{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -w #-}
module GTFS.Realtime.Internal.Com.Google.Transit.Realtime.Alert.Cause (Cause(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.List as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data Cause = UNKNOWN_CAUSE
           | OTHER_CAUSE
           | TECHNICAL_PROBLEM
           | STRIKE
           | DEMONSTRATION
           | ACCIDENT
           | HOLIDAY
           | WEATHER
           | MAINTENANCE
           | CONSTRUCTION
           | POLICE_ACTIVITY
           | MEDICAL_EMERGENCY
             deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable Cause

instance Prelude'.Bounded Cause where
  minBound = UNKNOWN_CAUSE
  maxBound = MEDICAL_EMERGENCY

instance P'.Default Cause where
  defaultValue = UNKNOWN_CAUSE

toMaybe'Enum :: Prelude'.Int -> P'.Maybe Cause
toMaybe'Enum 1 = Prelude'.Just UNKNOWN_CAUSE
toMaybe'Enum 2 = Prelude'.Just OTHER_CAUSE
toMaybe'Enum 3 = Prelude'.Just TECHNICAL_PROBLEM
toMaybe'Enum 4 = Prelude'.Just STRIKE
toMaybe'Enum 5 = Prelude'.Just DEMONSTRATION
toMaybe'Enum 6 = Prelude'.Just ACCIDENT
toMaybe'Enum 7 = Prelude'.Just HOLIDAY
toMaybe'Enum 8 = Prelude'.Just WEATHER
toMaybe'Enum 9 = Prelude'.Just MAINTENANCE
toMaybe'Enum 10 = Prelude'.Just CONSTRUCTION
toMaybe'Enum 11 = Prelude'.Just POLICE_ACTIVITY
toMaybe'Enum 12 = Prelude'.Just MEDICAL_EMERGENCY
toMaybe'Enum _ = Prelude'.Nothing

instance Prelude'.Enum Cause where
  fromEnum UNKNOWN_CAUSE = 1
  fromEnum OTHER_CAUSE = 2
  fromEnum TECHNICAL_PROBLEM = 3
  fromEnum STRIKE = 4
  fromEnum DEMONSTRATION = 5
  fromEnum ACCIDENT = 6
  fromEnum HOLIDAY = 7
  fromEnum WEATHER = 8
  fromEnum MAINTENANCE = 9
  fromEnum CONSTRUCTION = 10
  fromEnum POLICE_ACTIVITY = 11
  fromEnum MEDICAL_EMERGENCY = 12
  toEnum
   = P'.fromMaybe
      (Prelude'.error
        "hprotoc generated code: toEnum failure for type GTFS.Realtime.Internal.Com.Google.Transit.Realtime.Alert.Cause")
      . toMaybe'Enum
  succ UNKNOWN_CAUSE = OTHER_CAUSE
  succ OTHER_CAUSE = TECHNICAL_PROBLEM
  succ TECHNICAL_PROBLEM = STRIKE
  succ STRIKE = DEMONSTRATION
  succ DEMONSTRATION = ACCIDENT
  succ ACCIDENT = HOLIDAY
  succ HOLIDAY = WEATHER
  succ WEATHER = MAINTENANCE
  succ MAINTENANCE = CONSTRUCTION
  succ CONSTRUCTION = POLICE_ACTIVITY
  succ POLICE_ACTIVITY = MEDICAL_EMERGENCY
  succ _
   = Prelude'.error "hprotoc generated code: succ failure for type GTFS.Realtime.Internal.Com.Google.Transit.Realtime.Alert.Cause"
  pred OTHER_CAUSE = UNKNOWN_CAUSE
  pred TECHNICAL_PROBLEM = OTHER_CAUSE
  pred STRIKE = TECHNICAL_PROBLEM
  pred DEMONSTRATION = STRIKE
  pred ACCIDENT = DEMONSTRATION
  pred HOLIDAY = ACCIDENT
  pred WEATHER = HOLIDAY
  pred MAINTENANCE = WEATHER
  pred CONSTRUCTION = MAINTENANCE
  pred POLICE_ACTIVITY = CONSTRUCTION
  pred MEDICAL_EMERGENCY = POLICE_ACTIVITY
  pred _
   = Prelude'.error "hprotoc generated code: pred failure for type GTFS.Realtime.Internal.Com.Google.Transit.Realtime.Alert.Cause"

instance P'.Wire Cause where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'

instance P'.GPB Cause

instance P'.MessageAPI msg' (msg' -> Cause) Cause where
  getVal m' f' = f' m'

instance P'.ReflectEnum Cause where
  reflectEnum
   = [(1, "UNKNOWN_CAUSE", UNKNOWN_CAUSE), (2, "OTHER_CAUSE", OTHER_CAUSE), (3, "TECHNICAL_PROBLEM", TECHNICAL_PROBLEM),
      (4, "STRIKE", STRIKE), (5, "DEMONSTRATION", DEMONSTRATION), (6, "ACCIDENT", ACCIDENT), (7, "HOLIDAY", HOLIDAY),
      (8, "WEATHER", WEATHER), (9, "MAINTENANCE", MAINTENANCE), (10, "CONSTRUCTION", CONSTRUCTION),
      (11, "POLICE_ACTIVITY", POLICE_ACTIVITY), (12, "MEDICAL_EMERGENCY", MEDICAL_EMERGENCY)]
  reflectEnumInfo _
   = P'.EnumInfo
      (P'.makePNF (P'.pack ".transit_realtime.Alert.Cause") ["GTFS", "Realtime", "Internal"]
        ["Com", "Google", "Transit", "Realtime", "Alert"]
        "Cause")
      ["GTFS", "Realtime", "Internal", "Com", "Google", "Transit", "Realtime", "Alert", "Cause.hs"]
      [(1, "UNKNOWN_CAUSE"), (2, "OTHER_CAUSE"), (3, "TECHNICAL_PROBLEM"), (4, "STRIKE"), (5, "DEMONSTRATION"), (6, "ACCIDENT"),
       (7, "HOLIDAY"), (8, "WEATHER"), (9, "MAINTENANCE"), (10, "CONSTRUCTION"), (11, "POLICE_ACTIVITY"), (12, "MEDICAL_EMERGENCY")]
      Prelude'.False

instance P'.TextType Cause where
  tellT = P'.tellShow
  getT = P'.getRead