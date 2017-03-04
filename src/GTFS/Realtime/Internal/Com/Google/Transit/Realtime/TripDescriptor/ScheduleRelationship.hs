{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TripDescriptor.ScheduleRelationship (ScheduleRelationship(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data ScheduleRelationship = SCHEDULED
                          | ADDED
                          | UNSCHEDULED
                          | CANCELED
                          deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)

instance P'.Mergeable ScheduleRelationship

instance Prelude'.Bounded ScheduleRelationship where
  minBound = SCHEDULED
  maxBound = CANCELED

instance P'.Default ScheduleRelationship where
  defaultValue = SCHEDULED

toMaybe'Enum :: Prelude'.Int -> P'.Maybe ScheduleRelationship
toMaybe'Enum 0 = Prelude'.Just SCHEDULED
toMaybe'Enum 1 = Prelude'.Just ADDED
toMaybe'Enum 2 = Prelude'.Just UNSCHEDULED
toMaybe'Enum 3 = Prelude'.Just CANCELED
toMaybe'Enum _ = Prelude'.Nothing

instance Prelude'.Enum ScheduleRelationship where
  fromEnum SCHEDULED = 0
  fromEnum ADDED = 1
  fromEnum UNSCHEDULED = 2
  fromEnum CANCELED = 3
  toEnum
   = P'.fromMaybe
      (Prelude'.error
        "hprotoc generated code: toEnum failure for type GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TripDescriptor.ScheduleRelationship")
      . toMaybe'Enum
  succ SCHEDULED = ADDED
  succ ADDED = UNSCHEDULED
  succ UNSCHEDULED = CANCELED
  succ _
   = Prelude'.error
      "hprotoc generated code: succ failure for type GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TripDescriptor.ScheduleRelationship"
  pred ADDED = SCHEDULED
  pred UNSCHEDULED = ADDED
  pred CANCELED = UNSCHEDULED
  pred _
   = Prelude'.error
      "hprotoc generated code: pred failure for type GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TripDescriptor.ScheduleRelationship"

instance P'.Wire ScheduleRelationship where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'

instance P'.GPB ScheduleRelationship

instance P'.MessageAPI msg' (msg' -> ScheduleRelationship) ScheduleRelationship where
  getVal m' f' = f' m'

instance P'.ReflectEnum ScheduleRelationship where
  reflectEnum = [(0, "SCHEDULED", SCHEDULED), (1, "ADDED", ADDED), (2, "UNSCHEDULED", UNSCHEDULED), (3, "CANCELED", CANCELED)]
  reflectEnumInfo _
   = P'.EnumInfo
      (P'.makePNF (P'.pack ".transit_realtime.TripDescriptor.ScheduleRelationship") ["GTFS", "Realtime", "Internal"]
        ["Com", "Google", "Transit", "Realtime", "TripDescriptor"]
        "ScheduleRelationship")
      ["GTFS", "Realtime", "Internal", "Com", "Google", "Transit", "Realtime", "TripDescriptor", "ScheduleRelationship.hs"]
      [(0, "SCHEDULED"), (1, "ADDED"), (2, "UNSCHEDULED"), (3, "CANCELED")]

instance P'.TextType ScheduleRelationship where
  tellT = P'.tellShow
  getT = P'.getRead