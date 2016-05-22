{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Com.Google.Transit.Realtime.VehiclePosition.VehicleStopStatus (VehicleStopStatus(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data VehicleStopStatus = INCOMING_AT
                       | STOPPED_AT
                       | IN_TRANSIT_TO
                       deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable VehicleStopStatus
 
instance Prelude'.Bounded VehicleStopStatus where
  minBound = INCOMING_AT
  maxBound = IN_TRANSIT_TO
 
instance P'.Default VehicleStopStatus where
  defaultValue = INCOMING_AT
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe VehicleStopStatus
toMaybe'Enum 0 = Prelude'.Just INCOMING_AT
toMaybe'Enum 1 = Prelude'.Just STOPPED_AT
toMaybe'Enum 2 = Prelude'.Just IN_TRANSIT_TO
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum VehicleStopStatus where
  fromEnum INCOMING_AT = 0
  fromEnum STOPPED_AT = 1
  fromEnum IN_TRANSIT_TO = 2
  toEnum
   = P'.fromMaybe
      (Prelude'.error
        "hprotoc generated code: toEnum failure for type Com.Google.Transit.Realtime.VehiclePosition.VehicleStopStatus")
      . toMaybe'Enum
  succ INCOMING_AT = STOPPED_AT
  succ STOPPED_AT = IN_TRANSIT_TO
  succ _
   = Prelude'.error "hprotoc generated code: succ failure for type Com.Google.Transit.Realtime.VehiclePosition.VehicleStopStatus"
  pred STOPPED_AT = INCOMING_AT
  pred IN_TRANSIT_TO = STOPPED_AT
  pred _
   = Prelude'.error "hprotoc generated code: pred failure for type Com.Google.Transit.Realtime.VehiclePosition.VehicleStopStatus"
 
instance P'.Wire VehicleStopStatus where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB VehicleStopStatus
 
instance P'.MessageAPI msg' (msg' -> VehicleStopStatus) VehicleStopStatus where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum VehicleStopStatus where
  reflectEnum = [(0, "INCOMING_AT", INCOMING_AT), (1, "STOPPED_AT", STOPPED_AT), (2, "IN_TRANSIT_TO", IN_TRANSIT_TO)]
  reflectEnumInfo _
   = P'.EnumInfo
      (P'.makePNF (P'.pack ".transit_realtime.VehiclePosition.VehicleStopStatus") []
        ["Com", "Google", "Transit", "Realtime", "VehiclePosition"]
        "VehicleStopStatus")
      ["Com", "Google", "Transit", "Realtime", "VehiclePosition", "VehicleStopStatus.hs"]
      [(0, "INCOMING_AT"), (1, "STOPPED_AT"), (2, "IN_TRANSIT_TO")]
 
instance P'.TextType VehicleStopStatus where
  tellT = P'.tellShow
  getT = P'.getRead