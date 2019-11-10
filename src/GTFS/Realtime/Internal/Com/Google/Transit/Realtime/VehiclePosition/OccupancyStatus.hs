{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module GTFS.Realtime.Internal.Com.Google.Transit.Realtime.VehiclePosition.OccupancyStatus (OccupancyStatus(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data OccupancyStatus = EMPTY
                     | MANY_SEATS_AVAILABLE
                     | FEW_SEATS_AVAILABLE
                     | STANDING_ROOM_ONLY
                     | CRUSHED_STANDING_ROOM_ONLY
                     | FULL
                     | NOT_ACCEPTING_PASSENGERS
                       deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data,
                                 Prelude'.Generic)

instance P'.Mergeable OccupancyStatus

instance Prelude'.Bounded OccupancyStatus where
  minBound = EMPTY
  maxBound = NOT_ACCEPTING_PASSENGERS

instance P'.Default OccupancyStatus where
  defaultValue = EMPTY

toMaybe'Enum :: Prelude'.Int -> P'.Maybe OccupancyStatus
toMaybe'Enum 0 = Prelude'.Just EMPTY
toMaybe'Enum 1 = Prelude'.Just MANY_SEATS_AVAILABLE
toMaybe'Enum 2 = Prelude'.Just FEW_SEATS_AVAILABLE
toMaybe'Enum 3 = Prelude'.Just STANDING_ROOM_ONLY
toMaybe'Enum 4 = Prelude'.Just CRUSHED_STANDING_ROOM_ONLY
toMaybe'Enum 5 = Prelude'.Just FULL
toMaybe'Enum 6 = Prelude'.Just NOT_ACCEPTING_PASSENGERS
toMaybe'Enum _ = Prelude'.Nothing

instance Prelude'.Enum OccupancyStatus where
  fromEnum EMPTY = 0
  fromEnum MANY_SEATS_AVAILABLE = 1
  fromEnum FEW_SEATS_AVAILABLE = 2
  fromEnum STANDING_ROOM_ONLY = 3
  fromEnum CRUSHED_STANDING_ROOM_ONLY = 4
  fromEnum FULL = 5
  fromEnum NOT_ACCEPTING_PASSENGERS = 6
  toEnum
   = P'.fromMaybe
      (Prelude'.error
        "hprotoc generated code: toEnum failure for type GTFS.Realtime.Internal.Com.Google.Transit.Realtime.VehiclePosition.OccupancyStatus")
      . toMaybe'Enum
  succ EMPTY = MANY_SEATS_AVAILABLE
  succ MANY_SEATS_AVAILABLE = FEW_SEATS_AVAILABLE
  succ FEW_SEATS_AVAILABLE = STANDING_ROOM_ONLY
  succ STANDING_ROOM_ONLY = CRUSHED_STANDING_ROOM_ONLY
  succ CRUSHED_STANDING_ROOM_ONLY = FULL
  succ FULL = NOT_ACCEPTING_PASSENGERS
  succ _
   = Prelude'.error
      "hprotoc generated code: succ failure for type GTFS.Realtime.Internal.Com.Google.Transit.Realtime.VehiclePosition.OccupancyStatus"
  pred MANY_SEATS_AVAILABLE = EMPTY
  pred FEW_SEATS_AVAILABLE = MANY_SEATS_AVAILABLE
  pred STANDING_ROOM_ONLY = FEW_SEATS_AVAILABLE
  pred CRUSHED_STANDING_ROOM_ONLY = STANDING_ROOM_ONLY
  pred FULL = CRUSHED_STANDING_ROOM_ONLY
  pred NOT_ACCEPTING_PASSENGERS = FULL
  pred _
   = Prelude'.error
      "hprotoc generated code: pred failure for type GTFS.Realtime.Internal.Com.Google.Transit.Realtime.VehiclePosition.OccupancyStatus"

instance P'.Wire OccupancyStatus where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'

instance P'.GPB OccupancyStatus

instance P'.MessageAPI msg' (msg' -> OccupancyStatus) OccupancyStatus where
  getVal m' f' = f' m'

instance P'.ReflectEnum OccupancyStatus where
  reflectEnum
   = [(0, "EMPTY", EMPTY), (1, "MANY_SEATS_AVAILABLE", MANY_SEATS_AVAILABLE), (2, "FEW_SEATS_AVAILABLE", FEW_SEATS_AVAILABLE),
      (3, "STANDING_ROOM_ONLY", STANDING_ROOM_ONLY), (4, "CRUSHED_STANDING_ROOM_ONLY", CRUSHED_STANDING_ROOM_ONLY),
      (5, "FULL", FULL), (6, "NOT_ACCEPTING_PASSENGERS", NOT_ACCEPTING_PASSENGERS)]
  reflectEnumInfo _
   = P'.EnumInfo
      (P'.makePNF (P'.pack ".transit_realtime.VehiclePosition.OccupancyStatus") ["GTFS", "Realtime", "Internal"]
        ["Com", "Google", "Transit", "Realtime", "VehiclePosition"]
        "OccupancyStatus")
      ["GTFS", "Realtime", "Internal", "Com", "Google", "Transit", "Realtime", "VehiclePosition", "OccupancyStatus.hs"]
      [(0, "EMPTY"), (1, "MANY_SEATS_AVAILABLE"), (2, "FEW_SEATS_AVAILABLE"), (3, "STANDING_ROOM_ONLY"),
       (4, "CRUSHED_STANDING_ROOM_ONLY"), (5, "FULL"), (6, "NOT_ACCEPTING_PASSENGERS")]
      Prelude'.False

instance P'.TextType OccupancyStatus where
  tellT = P'.tellShow
  getT = P'.getRead