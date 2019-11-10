{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TripUpdate.StopTimeUpdate (StopTimeUpdate(..)) where
import Prelude ((+), (/), (++), (.), (==), (<=), (&&))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TripUpdate.StopTimeEvent
       as Com.Google.Transit.Realtime.TripUpdate (StopTimeEvent)
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TripUpdate.StopTimeUpdate.ScheduleRelationship
       as Com.Google.Transit.Realtime.TripUpdate.StopTimeUpdate (ScheduleRelationship)

data StopTimeUpdate = StopTimeUpdate{stop_sequence :: !(P'.Maybe P'.Word32), stop_id :: !(P'.Maybe P'.Utf8),
                                     arrival :: !(P'.Maybe Com.Google.Transit.Realtime.TripUpdate.StopTimeEvent),
                                     departure :: !(P'.Maybe Com.Google.Transit.Realtime.TripUpdate.StopTimeEvent),
                                     schedule_relationship ::
                                     !(P'.Maybe Com.Google.Transit.Realtime.TripUpdate.StopTimeUpdate.ScheduleRelationship),
                                     ext'field :: !(P'.ExtField), unknown'field :: !(P'.UnknownField)}
                      deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.ExtendMessage StopTimeUpdate where
  getExtField = ext'field
  putExtField e'f msg = msg{ext'field = e'f}
  validExtRanges msg = P'.extRanges (P'.reflectDescriptorInfo msg)

instance P'.UnknownMessage StopTimeUpdate where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}

instance P'.Mergeable StopTimeUpdate where
  mergeAppend (StopTimeUpdate x'1 x'2 x'3 x'4 x'5 x'6 x'7) (StopTimeUpdate y'1 y'2 y'3 y'4 y'5 y'6 y'7)
   = StopTimeUpdate (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
      (P'.mergeAppend x'7 y'7)

instance P'.Default StopTimeUpdate where
  defaultValue
   = StopTimeUpdate P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue (Prelude'.Just (Prelude'.read "SCHEDULED"))
      P'.defaultValue
      P'.defaultValue

instance P'.Wire StopTimeUpdate where
  wireSize ft' self'@(StopTimeUpdate x'1 x'2 x'3 x'4 x'5 x'6 x'7)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeOpt 1 13 x'1 + P'.wireSizeOpt 1 9 x'2 + P'.wireSizeOpt 1 11 x'3 + P'.wireSizeOpt 1 11 x'4 +
             P'.wireSizeOpt 1 14 x'5
             + P'.wireSizeExtField x'6
             + P'.wireSizeUnknownField x'7)
  wirePutWithSize ft' self'@(StopTimeUpdate x'1 x'2 x'3 x'4 x'5 x'6 x'7)
   = case ft' of
       10 -> put'Fields
       11 -> put'FieldsSized
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = P'.sequencePutWithSize
            [P'.wirePutOptWithSize 8 13 x'1, P'.wirePutOptWithSize 18 11 x'3, P'.wirePutOptWithSize 26 11 x'4,
             P'.wirePutOptWithSize 34 9 x'2, P'.wirePutOptWithSize 40 14 x'5, P'.wirePutExtFieldWithSize x'6,
             P'.wirePutUnknownFieldWithSize x'7]
        put'FieldsSized
         = let size' = Prelude'.fst (P'.runPutM put'Fields)
               put'Size
                = do
                    P'.putSize size'
                    Prelude'.return (P'.size'WireSize size')
            in P'.sequencePutWithSize [put'Size, put'Fields]
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith (P'.catch'Unknown' P'.loadUnknown update'Self)
       11 -> P'.getMessageWith (P'.catch'Unknown' P'.loadUnknown update'Self)
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{stop_sequence = Prelude'.Just new'Field}) (P'.wireGet 13)
             34 -> Prelude'.fmap (\ !new'Field -> old'Self{stop_id = Prelude'.Just new'Field}) (P'.wireGet 9)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{arrival = P'.mergeAppend (arrival old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             26 -> Prelude'.fmap
                    (\ !new'Field -> old'Self{departure = P'.mergeAppend (departure old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             40 -> Prelude'.fmap (\ !new'Field -> old'Self{schedule_relationship = Prelude'.Just new'Field}) (P'.wireGet 14)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in
                   if Prelude'.or [1000 <= field'Number && field'Number <= 1999] then
                    P'.loadExtension field'Number wire'Type old'Self else P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> StopTimeUpdate) StopTimeUpdate where
  getVal m' f' = f' m'

instance P'.GPB StopTimeUpdate

instance P'.ReflectDescriptor StopTimeUpdate where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8, 18, 26, 34, 40])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".transit_realtime.TripUpdate.StopTimeUpdate\", haskellPrefix = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"TripUpdate\"], baseName = MName \"StopTimeUpdate\"}, descFilePath = [\"GTFS\",\"Realtime\",\"Internal\",\"Com\",\"Google\",\"Transit\",\"Realtime\",\"TripUpdate\",\"StopTimeUpdate.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.TripUpdate.StopTimeUpdate.stop_sequence\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"TripUpdate\",MName \"StopTimeUpdate\"], baseName' = FName \"stop_sequence\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.TripUpdate.StopTimeUpdate.stop_id\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"TripUpdate\",MName \"StopTimeUpdate\"], baseName' = FName \"stop_id\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.TripUpdate.StopTimeUpdate.arrival\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"TripUpdate\",MName \"StopTimeUpdate\"], baseName' = FName \"arrival\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".transit_realtime.TripUpdate.StopTimeEvent\", haskellPrefix = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"TripUpdate\"], baseName = MName \"StopTimeEvent\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.TripUpdate.StopTimeUpdate.departure\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"TripUpdate\",MName \"StopTimeUpdate\"], baseName' = FName \"departure\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".transit_realtime.TripUpdate.StopTimeEvent\", haskellPrefix = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"TripUpdate\"], baseName = MName \"StopTimeEvent\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.TripUpdate.StopTimeUpdate.schedule_relationship\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"TripUpdate\",MName \"StopTimeUpdate\"], baseName' = FName \"schedule_relationship\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 40}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".transit_realtime.TripUpdate.StopTimeUpdate.ScheduleRelationship\", haskellPrefix = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"TripUpdate\",MName \"StopTimeUpdate\"], baseName = MName \"ScheduleRelationship\"}), hsRawDefault = Just \"SCHEDULED\", hsDefault = Just (HsDef'Enum \"SCHEDULED\")}], descOneofs = fromList [], keys = fromList [], extRanges = [(FieldId {getFieldId = 1000},FieldId {getFieldId = 1999})], knownKeys = fromList [], storeUnknown = True, lazyFields = False, makeLenses = False, jsonInstances = False}"

instance P'.TextType StopTimeUpdate where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg StopTimeUpdate where
  textPut msg
   = do
       P'.tellT "stop_sequence" (stop_sequence msg)
       P'.tellT "stop_id" (stop_id msg)
       P'.tellT "arrival" (arrival msg)
       P'.tellT "departure" (departure msg)
       P'.tellT "schedule_relationship" (schedule_relationship msg)
  textGet
   = do
       mods <- P'.sepEndBy
                (P'.choice [parse'stop_sequence, parse'stop_id, parse'arrival, parse'departure, parse'schedule_relationship])
                P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'stop_sequence
         = P'.try
            (do
               v <- P'.getT "stop_sequence"
               Prelude'.return (\ o -> o{stop_sequence = v}))
        parse'stop_id
         = P'.try
            (do
               v <- P'.getT "stop_id"
               Prelude'.return (\ o -> o{stop_id = v}))
        parse'arrival
         = P'.try
            (do
               v <- P'.getT "arrival"
               Prelude'.return (\ o -> o{arrival = v}))
        parse'departure
         = P'.try
            (do
               v <- P'.getT "departure"
               Prelude'.return (\ o -> o{departure = v}))
        parse'schedule_relationship
         = P'.try
            (do
               v <- P'.getT "schedule_relationship"
               Prelude'.return (\ o -> o{schedule_relationship = v}))