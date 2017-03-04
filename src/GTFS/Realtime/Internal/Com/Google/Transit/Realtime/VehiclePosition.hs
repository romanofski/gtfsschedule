{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module GTFS.Realtime.Internal.Com.Google.Transit.Realtime.VehiclePosition (VehiclePosition(..)) where
import Prelude ((+), (/), (==), (<=), (&&))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.Position as Com.Google.Transit.Realtime (Position)
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TripDescriptor as Com.Google.Transit.Realtime (TripDescriptor)
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.VehicleDescriptor as Com.Google.Transit.Realtime
       (VehicleDescriptor)
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.VehiclePosition.CongestionLevel
       as Com.Google.Transit.Realtime.VehiclePosition (CongestionLevel)
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.VehiclePosition.OccupancyStatus
       as Com.Google.Transit.Realtime.VehiclePosition (OccupancyStatus)
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.VehiclePosition.VehicleStopStatus
       as Com.Google.Transit.Realtime.VehiclePosition (VehicleStopStatus)

data VehiclePosition = VehiclePosition{trip :: !(P'.Maybe Com.Google.Transit.Realtime.TripDescriptor),
                                       vehicle :: !(P'.Maybe Com.Google.Transit.Realtime.VehicleDescriptor),
                                       position :: !(P'.Maybe Com.Google.Transit.Realtime.Position),
                                       current_stop_sequence :: !(P'.Maybe P'.Word32), stop_id :: !(P'.Maybe P'.Utf8),
                                       current_status :: !(P'.Maybe Com.Google.Transit.Realtime.VehiclePosition.VehicleStopStatus),
                                       timestamp :: !(P'.Maybe P'.Word64),
                                       congestion_level :: !(P'.Maybe Com.Google.Transit.Realtime.VehiclePosition.CongestionLevel),
                                       occupancy_status :: !(P'.Maybe Com.Google.Transit.Realtime.VehiclePosition.OccupancyStatus),
                                       ext'field :: !(P'.ExtField), unknown'field :: !(P'.UnknownField)}
                     deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)

instance P'.ExtendMessage VehiclePosition where
  getExtField = ext'field
  putExtField e'f msg = msg{ext'field = e'f}
  validExtRanges msg = P'.extRanges (P'.reflectDescriptorInfo msg)

instance P'.UnknownMessage VehiclePosition where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}

instance P'.Mergeable VehiclePosition where
  mergeAppend (VehiclePosition x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11)
   (VehiclePosition y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8 y'9 y'10 y'11)
   = VehiclePosition (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
      (P'.mergeAppend x'7 y'7)
      (P'.mergeAppend x'8 y'8)
      (P'.mergeAppend x'9 y'9)
      (P'.mergeAppend x'10 y'10)
      (P'.mergeAppend x'11 y'11)

instance P'.Default VehiclePosition where
  defaultValue
   = VehiclePosition P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
      (Prelude'.Just (Prelude'.read "IN_TRANSIT_TO"))
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue

instance P'.Wire VehiclePosition where
  wireSize ft' self'@(VehiclePosition x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeOpt 1 11 x'1 + P'.wireSizeOpt 1 11 x'2 + P'.wireSizeOpt 1 11 x'3 + P'.wireSizeOpt 1 13 x'4 +
             P'.wireSizeOpt 1 9 x'5
             + P'.wireSizeOpt 1 14 x'6
             + P'.wireSizeOpt 1 4 x'7
             + P'.wireSizeOpt 1 14 x'8
             + P'.wireSizeOpt 1 14 x'9
             + P'.wireSizeExtField x'10
             + P'.wireSizeUnknownField x'11)
  wirePut ft' self'@(VehiclePosition x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 10 11 x'1
             P'.wirePutOpt 18 11 x'3
             P'.wirePutOpt 24 13 x'4
             P'.wirePutOpt 32 14 x'6
             P'.wirePutOpt 40 4 x'7
             P'.wirePutOpt 48 14 x'8
             P'.wirePutOpt 58 9 x'5
             P'.wirePutOpt 66 11 x'2
             P'.wirePutOpt 72 14 x'9
             P'.wirePutExtField x'10
             P'.wirePutUnknownField x'11
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith (P'.catch'Unknown update'Self)
       11 -> P'.getMessageWith (P'.catch'Unknown update'Self)
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{trip = P'.mergeAppend (trip old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             66 -> Prelude'.fmap (\ !new'Field -> old'Self{vehicle = P'.mergeAppend (vehicle old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{position = P'.mergeAppend (position old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             24 -> Prelude'.fmap (\ !new'Field -> old'Self{current_stop_sequence = Prelude'.Just new'Field}) (P'.wireGet 13)
             58 -> Prelude'.fmap (\ !new'Field -> old'Self{stop_id = Prelude'.Just new'Field}) (P'.wireGet 9)
             32 -> Prelude'.fmap (\ !new'Field -> old'Self{current_status = Prelude'.Just new'Field}) (P'.wireGet 14)
             40 -> Prelude'.fmap (\ !new'Field -> old'Self{timestamp = Prelude'.Just new'Field}) (P'.wireGet 4)
             48 -> Prelude'.fmap (\ !new'Field -> old'Self{congestion_level = Prelude'.Just new'Field}) (P'.wireGet 14)
             72 -> Prelude'.fmap (\ !new'Field -> old'Self{occupancy_status = Prelude'.Just new'Field}) (P'.wireGet 14)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in
                   if Prelude'.or [1000 <= field'Number && field'Number <= 1999] then
                    P'.loadExtension field'Number wire'Type old'Self else P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> VehiclePosition) VehiclePosition where
  getVal m' f' = f' m'

instance P'.GPB VehiclePosition

instance P'.ReflectDescriptor VehiclePosition where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 18, 24, 32, 40, 48, 58, 66, 72])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".transit_realtime.VehiclePosition\", haskellPrefix = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\"], baseName = MName \"VehiclePosition\"}, descFilePath = [\"GTFS\",\"Realtime\",\"Internal\",\"Com\",\"Google\",\"Transit\",\"Realtime\",\"VehiclePosition.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.VehiclePosition.trip\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"VehiclePosition\"], baseName' = FName \"trip\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".transit_realtime.TripDescriptor\", haskellPrefix = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\"], baseName = MName \"TripDescriptor\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.VehiclePosition.vehicle\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"VehiclePosition\"], baseName' = FName \"vehicle\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 66}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".transit_realtime.VehicleDescriptor\", haskellPrefix = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\"], baseName = MName \"VehicleDescriptor\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.VehiclePosition.position\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"VehiclePosition\"], baseName' = FName \"position\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".transit_realtime.Position\", haskellPrefix = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\"], baseName = MName \"Position\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.VehiclePosition.current_stop_sequence\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"VehiclePosition\"], baseName' = FName \"current_stop_sequence\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.VehiclePosition.stop_id\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"VehiclePosition\"], baseName' = FName \"stop_id\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 7}, wireTag = WireTag {getWireTag = 58}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.VehiclePosition.current_status\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"VehiclePosition\"], baseName' = FName \"current_status\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 32}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".transit_realtime.VehiclePosition.VehicleStopStatus\", haskellPrefix = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"VehiclePosition\"], baseName = MName \"VehicleStopStatus\"}), hsRawDefault = Just \"IN_TRANSIT_TO\", hsDefault = Just (HsDef'Enum \"IN_TRANSIT_TO\")},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.VehiclePosition.timestamp\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"VehiclePosition\"], baseName' = FName \"timestamp\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 40}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 4}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.VehiclePosition.congestion_level\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"VehiclePosition\"], baseName' = FName \"congestion_level\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 48}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".transit_realtime.VehiclePosition.CongestionLevel\", haskellPrefix = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"VehiclePosition\"], baseName = MName \"CongestionLevel\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.VehiclePosition.occupancy_status\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"VehiclePosition\"], baseName' = FName \"occupancy_status\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 9}, wireTag = WireTag {getWireTag = 72}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".transit_realtime.VehiclePosition.OccupancyStatus\", haskellPrefix = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"VehiclePosition\"], baseName = MName \"OccupancyStatus\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [(FieldId {getFieldId = 1000},FieldId {getFieldId = 1999})], knownKeys = fromList [], storeUnknown = True, lazyFields = False, makeLenses = False}"

instance P'.TextType VehiclePosition where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg VehiclePosition where
  textPut msg
   = do
       P'.tellT "trip" (trip msg)
       P'.tellT "vehicle" (vehicle msg)
       P'.tellT "position" (position msg)
       P'.tellT "current_stop_sequence" (current_stop_sequence msg)
       P'.tellT "stop_id" (stop_id msg)
       P'.tellT "current_status" (current_status msg)
       P'.tellT "timestamp" (timestamp msg)
       P'.tellT "congestion_level" (congestion_level msg)
       P'.tellT "occupancy_status" (occupancy_status msg)
  textGet
   = do
       mods <- P'.sepEndBy
                (P'.choice
                  [parse'trip, parse'vehicle, parse'position, parse'current_stop_sequence, parse'stop_id, parse'current_status,
                   parse'timestamp, parse'congestion_level, parse'occupancy_status])
                P'.spaces
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
        parse'position
         = P'.try
            (do
               v <- P'.getT "position"
               Prelude'.return (\ o -> o{position = v}))
        parse'current_stop_sequence
         = P'.try
            (do
               v <- P'.getT "current_stop_sequence"
               Prelude'.return (\ o -> o{current_stop_sequence = v}))
        parse'stop_id
         = P'.try
            (do
               v <- P'.getT "stop_id"
               Prelude'.return (\ o -> o{stop_id = v}))
        parse'current_status
         = P'.try
            (do
               v <- P'.getT "current_status"
               Prelude'.return (\ o -> o{current_status = v}))
        parse'timestamp
         = P'.try
            (do
               v <- P'.getT "timestamp"
               Prelude'.return (\ o -> o{timestamp = v}))
        parse'congestion_level
         = P'.try
            (do
               v <- P'.getT "congestion_level"
               Prelude'.return (\ o -> o{congestion_level = v}))
        parse'occupancy_status
         = P'.try
            (do
               v <- P'.getT "occupancy_status"
               Prelude'.return (\ o -> o{occupancy_status = v}))