{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Com.Google.Transit.Realtime.TripUpdate (TripUpdate(..)) where
import Prelude ((+), (/), (==), (<=), (&&))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Com.Google.Transit.Realtime.TripDescriptor as Com.Google.Transit.Realtime (TripDescriptor)
import qualified Com.Google.Transit.Realtime.TripUpdate.StopTimeUpdate as Com.Google.Transit.Realtime.TripUpdate (StopTimeUpdate)
import qualified Com.Google.Transit.Realtime.VehicleDescriptor as Com.Google.Transit.Realtime (VehicleDescriptor)
 
data TripUpdate = TripUpdate{trip :: !(Com.Google.Transit.Realtime.TripDescriptor),
                             vehicle :: !(P'.Maybe Com.Google.Transit.Realtime.VehicleDescriptor),
                             stop_time_update :: !(P'.Seq Com.Google.Transit.Realtime.TripUpdate.StopTimeUpdate),
                             timestamp :: !(P'.Maybe P'.Word64), delay :: !(P'.Maybe P'.Int32), ext'field :: !(P'.ExtField),
                             unknown'field :: !(P'.UnknownField)}
                deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.ExtendMessage TripUpdate where
  getExtField = ext'field
  putExtField e'f msg = msg{ext'field = e'f}
  validExtRanges msg = P'.extRanges (P'.reflectDescriptorInfo msg)
 
instance P'.UnknownMessage TripUpdate where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable TripUpdate where
  mergeAppend (TripUpdate x'1 x'2 x'3 x'4 x'5 x'6 x'7) (TripUpdate y'1 y'2 y'3 y'4 y'5 y'6 y'7)
   = TripUpdate (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
      (P'.mergeAppend x'7 y'7)
 
instance P'.Default TripUpdate where
  defaultValue
   = TripUpdate P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire TripUpdate where
  wireSize ft' self'@(TripUpdate x'1 x'2 x'3 x'4 x'5 x'6 x'7)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeReq 1 11 x'1 + P'.wireSizeOpt 1 11 x'2 + P'.wireSizeRep 1 11 x'3 + P'.wireSizeOpt 1 4 x'4 +
             P'.wireSizeOpt 1 5 x'5
             + P'.wireSizeExtField x'6
             + P'.wireSizeUnknownField x'7)
  wirePut ft' self'@(TripUpdate x'1 x'2 x'3 x'4 x'5 x'6 x'7)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutReq 10 11 x'1
             P'.wirePutRep 18 11 x'3
             P'.wirePutOpt 26 11 x'2
             P'.wirePutOpt 32 4 x'4
             P'.wirePutOpt 40 5 x'5
             P'.wirePutExtField x'6
             P'.wirePutUnknownField x'7
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith (P'.catch'Unknown update'Self)
       11 -> P'.getMessageWith (P'.catch'Unknown update'Self)
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{trip = P'.mergeAppend (trip old'Self) (new'Field)}) (P'.wireGet 11)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{vehicle = P'.mergeAppend (vehicle old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{stop_time_update = P'.append (stop_time_update old'Self) new'Field})
                    (P'.wireGet 11)
             32 -> Prelude'.fmap (\ !new'Field -> old'Self{timestamp = Prelude'.Just new'Field}) (P'.wireGet 4)
             40 -> Prelude'.fmap (\ !new'Field -> old'Self{delay = Prelude'.Just new'Field}) (P'.wireGet 5)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in
                   if Prelude'.or [1000 <= field'Number && field'Number <= 1999] then
                    P'.loadExtension field'Number wire'Type old'Self else P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> TripUpdate) TripUpdate where
  getVal m' f' = f' m'
 
instance P'.GPB TripUpdate
 
instance P'.ReflectDescriptor TripUpdate where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [10]) (P'.fromDistinctAscList [10, 18, 26, 32, 40])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".transit_realtime.TripUpdate\", haskellPrefix = [], parentModule = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\"], baseName = MName \"TripUpdate\"}, descFilePath = [\"Com\",\"Google\",\"Transit\",\"Realtime\",\"TripUpdate.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.TripUpdate.trip\", haskellPrefix' = [], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"TripUpdate\"], baseName' = FName \"trip\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".transit_realtime.TripDescriptor\", haskellPrefix = [], parentModule = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\"], baseName = MName \"TripDescriptor\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.TripUpdate.vehicle\", haskellPrefix' = [], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"TripUpdate\"], baseName' = FName \"vehicle\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".transit_realtime.VehicleDescriptor\", haskellPrefix = [], parentModule = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\"], baseName = MName \"VehicleDescriptor\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.TripUpdate.stop_time_update\", haskellPrefix' = [], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"TripUpdate\"], baseName' = FName \"stop_time_update\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".transit_realtime.TripUpdate.StopTimeUpdate\", haskellPrefix = [], parentModule = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"TripUpdate\"], baseName = MName \"StopTimeUpdate\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.TripUpdate.timestamp\", haskellPrefix' = [], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"TripUpdate\"], baseName' = FName \"timestamp\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 32}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 4}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.TripUpdate.delay\", haskellPrefix' = [], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"TripUpdate\"], baseName' = FName \"delay\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 40}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [(FieldId {getFieldId = 1000},FieldId {getFieldId = 1999})], knownKeys = fromList [], storeUnknown = True, lazyFields = False}"
 
instance P'.TextType TripUpdate where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg TripUpdate where
  textPut msg
   = do
       P'.tellT "trip" (trip msg)
       P'.tellT "vehicle" (vehicle msg)
       P'.tellT "stop_time_update" (stop_time_update msg)
       P'.tellT "timestamp" (timestamp msg)
       P'.tellT "delay" (delay msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'trip, parse'vehicle, parse'stop_time_update, parse'timestamp, parse'delay]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'trip
         = P'.try
            (do
               v <- P'.getT "trip"
               Prelude'.return (\ o -> o{trip = v}))
        parse'vehicle
         = P'.try
            (do
               v <- P'.getT "vehicle"
               Prelude'.return (\ o -> o{vehicle = v}))
        parse'stop_time_update
         = P'.try
            (do
               v <- P'.getT "stop_time_update"
               Prelude'.return (\ o -> o{stop_time_update = P'.append (stop_time_update o) v}))
        parse'timestamp
         = P'.try
            (do
               v <- P'.getT "timestamp"
               Prelude'.return (\ o -> o{timestamp = v}))
        parse'delay
         = P'.try
            (do
               v <- P'.getT "delay"
               Prelude'.return (\ o -> o{delay = v}))