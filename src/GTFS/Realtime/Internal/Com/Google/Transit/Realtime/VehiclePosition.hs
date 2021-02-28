{-# LANGUAGE TemplateHaskell, BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses,
 OverloadedStrings #-}
{-# OPTIONS_GHC  -w #-}
module GTFS.Realtime.Internal.Com.Google.Transit.Realtime.VehiclePosition
       (VehiclePosition(..), trip, vehicle, position, current_stop_sequence, stop_id, current_status, timestamp, congestion_level,
        occupancy_status)
       where
import Prelude ((+), (/), (++), (.), (==), (<=), (&&))
import qualified Prelude as Prelude'
import qualified Data.List as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Control.Lens.TH
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

data VehiclePosition = VehiclePosition{_trip :: !(P'.Maybe Com.Google.Transit.Realtime.TripDescriptor),
                                       _vehicle :: !(P'.Maybe Com.Google.Transit.Realtime.VehicleDescriptor),
                                       _position :: !(P'.Maybe Com.Google.Transit.Realtime.Position),
                                       _current_stop_sequence :: !(P'.Maybe P'.Word32), _stop_id :: !(P'.Maybe P'.Utf8),
                                       _current_status :: !(P'.Maybe Com.Google.Transit.Realtime.VehiclePosition.VehicleStopStatus),
                                       _timestamp :: !(P'.Maybe P'.Word64),
                                       _congestion_level :: !(P'.Maybe Com.Google.Transit.Realtime.VehiclePosition.CongestionLevel),
                                       _occupancy_status :: !(P'.Maybe Com.Google.Transit.Realtime.VehiclePosition.OccupancyStatus),
                                       _ext'field :: !(P'.ExtField), _unknown'field :: !(P'.UnknownField)}
                       deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

Control.Lens.TH.makeLenses ''VehiclePosition

instance P'.ExtendMessage VehiclePosition where
  getExtField = _ext'field
  putExtField e'f msg = msg{_ext'field = e'f}
  validExtRanges msg = P'.extRanges (P'.reflectDescriptorInfo msg)

instance P'.UnknownMessage VehiclePosition where
  getUnknownField = _unknown'field
  putUnknownField u'f msg = msg{_unknown'field = u'f}

instance P'.Mergeable VehiclePosition where
  mergeAppend (VehiclePosition x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11)
   (VehiclePosition y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8 y'9 y'10 y'11)
   = let !z'1 = P'.mergeAppend x'1 y'1
         !z'2 = P'.mergeAppend x'2 y'2
         !z'3 = P'.mergeAppend x'3 y'3
         !z'4 = P'.mergeAppend x'4 y'4
         !z'5 = P'.mergeAppend x'5 y'5
         !z'6 = P'.mergeAppend x'6 y'6
         !z'7 = P'.mergeAppend x'7 y'7
         !z'8 = P'.mergeAppend x'8 y'8
         !z'9 = P'.mergeAppend x'9 y'9
         !z'10 = P'.mergeAppend x'10 y'10
         !z'11 = P'.mergeAppend x'11 y'11
      in VehiclePosition z'1 z'2 z'3 z'4 z'5 z'6 z'7 z'8 z'9 z'10 z'11

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
  wirePutWithSize ft' self'@(VehiclePosition x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11)
   = case ft' of
       10 -> put'Fields
       11 -> put'FieldsSized
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = P'.sequencePutWithSize
            [P'.wirePutOptWithSize 10 11 x'1, P'.wirePutOptWithSize 18 11 x'3, P'.wirePutOptWithSize 24 13 x'4,
             P'.wirePutOptWithSize 32 14 x'6, P'.wirePutOptWithSize 40 4 x'7, P'.wirePutOptWithSize 48 14 x'8,
             P'.wirePutOptWithSize 58 9 x'5, P'.wirePutOptWithSize 66 11 x'2, P'.wirePutOptWithSize 72 14 x'9,
             P'.wirePutExtFieldWithSize x'10, P'.wirePutUnknownFieldWithSize x'11]
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
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{_trip = P'.mergeAppend (_trip old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             66 -> Prelude'.fmap (\ !new'Field -> old'Self{_vehicle = P'.mergeAppend (_vehicle old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             18 -> Prelude'.fmap
                    (\ !new'Field -> old'Self{_position = P'.mergeAppend (_position old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             24 -> Prelude'.fmap (\ !new'Field -> old'Self{_current_stop_sequence = Prelude'.Just new'Field}) (P'.wireGet 13)
             58 -> Prelude'.fmap (\ !new'Field -> old'Self{_stop_id = Prelude'.Just new'Field}) (P'.wireGet 9)
             32 -> Prelude'.fmap (\ !new'Field -> old'Self{_current_status = Prelude'.Just new'Field}) (P'.wireGet 14)
             40 -> Prelude'.fmap (\ !new'Field -> old'Self{_timestamp = Prelude'.Just new'Field}) (P'.wireGet 4)
             48 -> Prelude'.fmap (\ !new'Field -> old'Self{_congestion_level = Prelude'.Just new'Field}) (P'.wireGet 14)
             72 -> Prelude'.fmap (\ !new'Field -> old'Self{_occupancy_status = Prelude'.Just new'Field}) (P'.wireGet 14)
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
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".transit_realtime.VehiclePosition\", haskellPrefix = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\"], baseName = MName \"VehiclePosition\"}, descFilePath = [\"GTFS\",\"Realtime\",\"Internal\",\"Com\",\"Google\",\"Transit\",\"Realtime\",\"VehiclePosition.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.VehiclePosition.trip\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"VehiclePosition\"], baseName' = FName \"trip\", baseNamePrefix' = \"_\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".transit_realtime.TripDescriptor\", haskellPrefix = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\"], baseName = MName \"TripDescriptor\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.VehiclePosition.vehicle\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"VehiclePosition\"], baseName' = FName \"vehicle\", baseNamePrefix' = \"_\"}, fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 66}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".transit_realtime.VehicleDescriptor\", haskellPrefix = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\"], baseName = MName \"VehicleDescriptor\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.VehiclePosition.position\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"VehiclePosition\"], baseName' = FName \"position\", baseNamePrefix' = \"_\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".transit_realtime.Position\", haskellPrefix = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\"], baseName = MName \"Position\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.VehiclePosition.current_stop_sequence\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"VehiclePosition\"], baseName' = FName \"current_stop_sequence\", baseNamePrefix' = \"_\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.VehiclePosition.stop_id\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"VehiclePosition\"], baseName' = FName \"stop_id\", baseNamePrefix' = \"_\"}, fieldNumber = FieldId {getFieldId = 7}, wireTag = WireTag {getWireTag = 58}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.VehiclePosition.current_status\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"VehiclePosition\"], baseName' = FName \"current_status\", baseNamePrefix' = \"_\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 32}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".transit_realtime.VehiclePosition.VehicleStopStatus\", haskellPrefix = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"VehiclePosition\"], baseName = MName \"VehicleStopStatus\"}), hsRawDefault = Just \"IN_TRANSIT_TO\", hsDefault = Just (HsDef'Enum \"IN_TRANSIT_TO\")},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.VehiclePosition.timestamp\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"VehiclePosition\"], baseName' = FName \"timestamp\", baseNamePrefix' = \"_\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 40}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 4}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.VehiclePosition.congestion_level\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"VehiclePosition\"], baseName' = FName \"congestion_level\", baseNamePrefix' = \"_\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 48}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".transit_realtime.VehiclePosition.CongestionLevel\", haskellPrefix = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"VehiclePosition\"], baseName = MName \"CongestionLevel\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.VehiclePosition.occupancy_status\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"VehiclePosition\"], baseName' = FName \"occupancy_status\", baseNamePrefix' = \"_\"}, fieldNumber = FieldId {getFieldId = 9}, wireTag = WireTag {getWireTag = 72}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".transit_realtime.VehiclePosition.OccupancyStatus\", haskellPrefix = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"VehiclePosition\"], baseName = MName \"OccupancyStatus\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [(FieldId {getFieldId = 1000},FieldId {getFieldId = 1999})], knownKeys = fromList [], storeUnknown = True, lazyFields = False, makeLenses = True, jsonInstances = False}"

instance P'.TextType VehiclePosition where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg VehiclePosition where
  textPut msg
   = do
       P'.tellT "trip" (_trip msg)
       P'.tellT "vehicle" (_vehicle msg)
       P'.tellT "position" (_position msg)
       P'.tellT "current_stop_sequence" (_current_stop_sequence msg)
       P'.tellT "stop_id" (_stop_id msg)
       P'.tellT "current_status" (_current_status msg)
       P'.tellT "timestamp" (_timestamp msg)
       P'.tellT "congestion_level" (_congestion_level msg)
       P'.tellT "occupancy_status" (_occupancy_status msg)
  textGet
   = do
       mods <- P'.sepEndBy
                (P'.choice
                  [parse'_trip, parse'_vehicle, parse'_position, parse'_current_stop_sequence, parse'_stop_id,
                   parse'_current_status, parse'_timestamp, parse'_congestion_level, parse'_occupancy_status])
                P'.spaces
       Prelude'.return (Prelude'.foldl' (\ v f -> f v) P'.defaultValue mods)
    where
        parse'_trip = Prelude'.fmap (\ v o -> o{_trip = v}) (P'.try (P'.getT "trip"))
        parse'_vehicle = Prelude'.fmap (\ v o -> o{_vehicle = v}) (P'.try (P'.getT "vehicle"))
        parse'_position = Prelude'.fmap (\ v o -> o{_position = v}) (P'.try (P'.getT "position"))
        parse'_current_stop_sequence
         = Prelude'.fmap (\ v o -> o{_current_stop_sequence = v}) (P'.try (P'.getT "current_stop_sequence"))
        parse'_stop_id = Prelude'.fmap (\ v o -> o{_stop_id = v}) (P'.try (P'.getT "stop_id"))
        parse'_current_status = Prelude'.fmap (\ v o -> o{_current_status = v}) (P'.try (P'.getT "current_status"))
        parse'_timestamp = Prelude'.fmap (\ v o -> o{_timestamp = v}) (P'.try (P'.getT "timestamp"))
        parse'_congestion_level = Prelude'.fmap (\ v o -> o{_congestion_level = v}) (P'.try (P'.getT "congestion_level"))
        parse'_occupancy_status = Prelude'.fmap (\ v o -> o{_occupancy_status = v}) (P'.try (P'.getT "occupancy_status"))