{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Com.Google.Transit.Realtime.Position (Position(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data Position = Position{latitude :: !(P'.Float), longitude :: !(P'.Float), bearing :: !(P'.Maybe P'.Float),
                         odometer :: !(P'.Maybe P'.Double), speed :: !(P'.Maybe P'.Float), unknown'field :: !(P'.UnknownField)}
              deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.UnknownMessage Position where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable Position where
  mergeAppend (Position x'1 x'2 x'3 x'4 x'5 x'6) (Position y'1 y'2 y'3 y'4 y'5 y'6)
   = Position (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
 
instance P'.Default Position where
  defaultValue = Position P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire Position where
  wireSize ft' self'@(Position x'1 x'2 x'3 x'4 x'5 x'6)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeReq 1 2 x'1 + P'.wireSizeReq 1 2 x'2 + P'.wireSizeOpt 1 2 x'3 + P'.wireSizeOpt 1 1 x'4 +
             P'.wireSizeOpt 1 2 x'5
             + P'.wireSizeUnknownField x'6)
  wirePut ft' self'@(Position x'1 x'2 x'3 x'4 x'5 x'6)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutReq 13 2 x'1
             P'.wirePutReq 21 2 x'2
             P'.wirePutOpt 29 2 x'3
             P'.wirePutOpt 33 1 x'4
             P'.wirePutOpt 45 2 x'5
             P'.wirePutUnknownField x'6
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith (P'.catch'Unknown update'Self)
       11 -> P'.getMessageWith (P'.catch'Unknown update'Self)
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             13 -> Prelude'.fmap (\ !new'Field -> old'Self{latitude = new'Field}) (P'.wireGet 2)
             21 -> Prelude'.fmap (\ !new'Field -> old'Self{longitude = new'Field}) (P'.wireGet 2)
             29 -> Prelude'.fmap (\ !new'Field -> old'Self{bearing = Prelude'.Just new'Field}) (P'.wireGet 2)
             33 -> Prelude'.fmap (\ !new'Field -> old'Self{odometer = Prelude'.Just new'Field}) (P'.wireGet 1)
             45 -> Prelude'.fmap (\ !new'Field -> old'Self{speed = Prelude'.Just new'Field}) (P'.wireGet 2)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> Position) Position where
  getVal m' f' = f' m'
 
instance P'.GPB Position
 
instance P'.ReflectDescriptor Position where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [13, 21]) (P'.fromDistinctAscList [13, 21, 29, 33, 45])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".transit_realtime.Position\", haskellPrefix = [], parentModule = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\"], baseName = MName \"Position\"}, descFilePath = [\"Com\",\"Google\",\"Transit\",\"Realtime\",\"Position.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.Position.latitude\", haskellPrefix' = [], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"Position\"], baseName' = FName \"latitude\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 13}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 2}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.Position.longitude\", haskellPrefix' = [], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"Position\"], baseName' = FName \"longitude\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 21}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 2}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.Position.bearing\", haskellPrefix' = [], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"Position\"], baseName' = FName \"bearing\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 29}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 2}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.Position.odometer\", haskellPrefix' = [], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"Position\"], baseName' = FName \"odometer\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 33}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.Position.speed\", haskellPrefix' = [], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"Position\"], baseName' = FName \"speed\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 45}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 2}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True, lazyFields = False}"
 
instance P'.TextType Position where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg Position where
  textPut msg
   = do
       P'.tellT "latitude" (latitude msg)
       P'.tellT "longitude" (longitude msg)
       P'.tellT "bearing" (bearing msg)
       P'.tellT "odometer" (odometer msg)
       P'.tellT "speed" (speed msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'latitude, parse'longitude, parse'bearing, parse'odometer, parse'speed]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'latitude
         = P'.try
            (do
               v <- P'.getT "latitude"
               Prelude'.return (\ o -> o{latitude = v}))
        parse'longitude
         = P'.try
            (do
               v <- P'.getT "longitude"
               Prelude'.return (\ o -> o{longitude = v}))
        parse'bearing
         = P'.try
            (do
               v <- P'.getT "bearing"
               Prelude'.return (\ o -> o{bearing = v}))
        parse'odometer
         = P'.try
            (do
               v <- P'.getT "odometer"
               Prelude'.return (\ o -> o{odometer = v}))
        parse'speed
         = P'.try
            (do
               v <- P'.getT "speed"
               Prelude'.return (\ o -> o{speed = v}))