{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Com.Google.Transit.Realtime.TripUpdate.StopTimeUpdate.ScheduleRelationship (ScheduleRelationship(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data ScheduleRelationship = SCHEDULED
                          | SKIPPED
                          | NO_DATA
                          deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable ScheduleRelationship
 
instance Prelude'.Bounded ScheduleRelationship where
  minBound = SCHEDULED
  maxBound = NO_DATA
 
instance P'.Default ScheduleRelationship where
  defaultValue = SCHEDULED
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe ScheduleRelationship
toMaybe'Enum 0 = Prelude'.Just SCHEDULED
toMaybe'Enum 1 = Prelude'.Just SKIPPED
toMaybe'Enum 2 = Prelude'.Just NO_DATA
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum ScheduleRelationship where
  fromEnum SCHEDULED = 0
  fromEnum SKIPPED = 1
  fromEnum NO_DATA = 2
  toEnum
   = P'.fromMaybe
      (Prelude'.error
        "hprotoc generated code: toEnum failure for type Com.Google.Transit.Realtime.TripUpdate.StopTimeUpdate.ScheduleRelationship")
      . toMaybe'Enum
  succ SCHEDULED = SKIPPED
  succ SKIPPED = NO_DATA
  succ _
   = Prelude'.error
      "hprotoc generated code: succ failure for type Com.Google.Transit.Realtime.TripUpdate.StopTimeUpdate.ScheduleRelationship"
  pred SKIPPED = SCHEDULED
  pred NO_DATA = SKIPPED
  pred _
   = Prelude'.error
      "hprotoc generated code: pred failure for type Com.Google.Transit.Realtime.TripUpdate.StopTimeUpdate.ScheduleRelationship"
 
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
  reflectEnum = [(0, "SCHEDULED", SCHEDULED), (1, "SKIPPED", SKIPPED), (2, "NO_DATA", NO_DATA)]
  reflectEnumInfo _
   = P'.EnumInfo
      (P'.makePNF (P'.pack ".transit_realtime.TripUpdate.StopTimeUpdate.ScheduleRelationship") []
        ["Com", "Google", "Transit", "Realtime", "TripUpdate", "StopTimeUpdate"]
        "ScheduleRelationship")
      ["Com", "Google", "Transit", "Realtime", "TripUpdate", "StopTimeUpdate", "ScheduleRelationship.hs"]
      [(0, "SCHEDULED"), (1, "SKIPPED"), (2, "NO_DATA")]
 
instance P'.TextType ScheduleRelationship where
  tellT = P'.tellShow
  getT = P'.getRead