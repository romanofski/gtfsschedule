{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module GTFS.Realtime.Internal.Com.Google.Transit.Realtime.VehiclePosition.CongestionLevel (CongestionLevel(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data CongestionLevel = UNKNOWN_CONGESTION_LEVEL
                     | RUNNING_SMOOTHLY
                     | STOP_AND_GO
                     | CONGESTION
                     | SEVERE_CONGESTION
                       deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data,
                                 Prelude'.Generic)

instance P'.Mergeable CongestionLevel

instance Prelude'.Bounded CongestionLevel where
  minBound = UNKNOWN_CONGESTION_LEVEL
  maxBound = SEVERE_CONGESTION

instance P'.Default CongestionLevel where
  defaultValue = UNKNOWN_CONGESTION_LEVEL

toMaybe'Enum :: Prelude'.Int -> P'.Maybe CongestionLevel
toMaybe'Enum 0 = Prelude'.Just UNKNOWN_CONGESTION_LEVEL
toMaybe'Enum 1 = Prelude'.Just RUNNING_SMOOTHLY
toMaybe'Enum 2 = Prelude'.Just STOP_AND_GO
toMaybe'Enum 3 = Prelude'.Just CONGESTION
toMaybe'Enum 4 = Prelude'.Just SEVERE_CONGESTION
toMaybe'Enum _ = Prelude'.Nothing

instance Prelude'.Enum CongestionLevel where
  fromEnum UNKNOWN_CONGESTION_LEVEL = 0
  fromEnum RUNNING_SMOOTHLY = 1
  fromEnum STOP_AND_GO = 2
  fromEnum CONGESTION = 3
  fromEnum SEVERE_CONGESTION = 4
  toEnum
   = P'.fromMaybe
      (Prelude'.error
        "hprotoc generated code: toEnum failure for type GTFS.Realtime.Internal.Com.Google.Transit.Realtime.VehiclePosition.CongestionLevel")
      . toMaybe'Enum
  succ UNKNOWN_CONGESTION_LEVEL = RUNNING_SMOOTHLY
  succ RUNNING_SMOOTHLY = STOP_AND_GO
  succ STOP_AND_GO = CONGESTION
  succ CONGESTION = SEVERE_CONGESTION
  succ _
   = Prelude'.error
      "hprotoc generated code: succ failure for type GTFS.Realtime.Internal.Com.Google.Transit.Realtime.VehiclePosition.CongestionLevel"
  pred RUNNING_SMOOTHLY = UNKNOWN_CONGESTION_LEVEL
  pred STOP_AND_GO = RUNNING_SMOOTHLY
  pred CONGESTION = STOP_AND_GO
  pred SEVERE_CONGESTION = CONGESTION
  pred _
   = Prelude'.error
      "hprotoc generated code: pred failure for type GTFS.Realtime.Internal.Com.Google.Transit.Realtime.VehiclePosition.CongestionLevel"

instance P'.Wire CongestionLevel where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'

instance P'.GPB CongestionLevel

instance P'.MessageAPI msg' (msg' -> CongestionLevel) CongestionLevel where
  getVal m' f' = f' m'

instance P'.ReflectEnum CongestionLevel where
  reflectEnum
   = [(0, "UNKNOWN_CONGESTION_LEVEL", UNKNOWN_CONGESTION_LEVEL), (1, "RUNNING_SMOOTHLY", RUNNING_SMOOTHLY),
      (2, "STOP_AND_GO", STOP_AND_GO), (3, "CONGESTION", CONGESTION), (4, "SEVERE_CONGESTION", SEVERE_CONGESTION)]
  reflectEnumInfo _
   = P'.EnumInfo
      (P'.makePNF (P'.pack ".transit_realtime.VehiclePosition.CongestionLevel") ["GTFS", "Realtime", "Internal"]
        ["Com", "Google", "Transit", "Realtime", "VehiclePosition"]
        "CongestionLevel")
      ["GTFS", "Realtime", "Internal", "Com", "Google", "Transit", "Realtime", "VehiclePosition", "CongestionLevel.hs"]
      [(0, "UNKNOWN_CONGESTION_LEVEL"), (1, "RUNNING_SMOOTHLY"), (2, "STOP_AND_GO"), (3, "CONGESTION"), (4, "SEVERE_CONGESTION")]
      Prelude'.False

instance P'.TextType CongestionLevel where
  tellT = P'.tellShow
  getT = P'.getRead