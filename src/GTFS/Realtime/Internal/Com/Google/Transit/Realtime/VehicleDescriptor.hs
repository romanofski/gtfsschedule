{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module GTFS.Realtime.Internal.Com.Google.Transit.Realtime.VehicleDescriptor (VehicleDescriptor(..)) where
import Prelude ((+), (/), (++), (.), (==), (<=), (&&))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data VehicleDescriptor = VehicleDescriptor{id :: !(P'.Maybe P'.Utf8), label :: !(P'.Maybe P'.Utf8),
                                           license_plate :: !(P'.Maybe P'.Utf8), ext'field :: !(P'.ExtField),
                                           unknown'field :: !(P'.UnknownField)}
                         deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.ExtendMessage VehicleDescriptor where
  getExtField = ext'field
  putExtField e'f msg = msg{ext'field = e'f}
  validExtRanges msg = P'.extRanges (P'.reflectDescriptorInfo msg)

instance P'.UnknownMessage VehicleDescriptor where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}

instance P'.Mergeable VehicleDescriptor where
  mergeAppend (VehicleDescriptor x'1 x'2 x'3 x'4 x'5) (VehicleDescriptor y'1 y'2 y'3 y'4 y'5)
   = VehicleDescriptor (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)

instance P'.Default VehicleDescriptor where
  defaultValue = VehicleDescriptor P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue

instance P'.Wire VehicleDescriptor where
  wireSize ft' self'@(VehicleDescriptor x'1 x'2 x'3 x'4 x'5)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeOpt 1 9 x'1 + P'.wireSizeOpt 1 9 x'2 + P'.wireSizeOpt 1 9 x'3 + P'.wireSizeExtField x'4 +
             P'.wireSizeUnknownField x'5)
  wirePutWithSize ft' self'@(VehicleDescriptor x'1 x'2 x'3 x'4 x'5)
   = case ft' of
       10 -> put'Fields
       11 -> put'FieldsSized
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = P'.sequencePutWithSize
            [P'.wirePutOptWithSize 10 9 x'1, P'.wirePutOptWithSize 18 9 x'2, P'.wirePutOptWithSize 26 9 x'3,
             P'.wirePutExtFieldWithSize x'4, P'.wirePutUnknownFieldWithSize x'5]
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
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{id = Prelude'.Just new'Field}) (P'.wireGet 9)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{label = Prelude'.Just new'Field}) (P'.wireGet 9)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{license_plate = Prelude'.Just new'Field}) (P'.wireGet 9)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in
                   if Prelude'.or [1000 <= field'Number && field'Number <= 1999] then
                    P'.loadExtension field'Number wire'Type old'Self else P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> VehicleDescriptor) VehicleDescriptor where
  getVal m' f' = f' m'

instance P'.GPB VehicleDescriptor

instance P'.ReflectDescriptor VehicleDescriptor where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 18, 26])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".transit_realtime.VehicleDescriptor\", haskellPrefix = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\"], baseName = MName \"VehicleDescriptor\"}, descFilePath = [\"GTFS\",\"Realtime\",\"Internal\",\"Com\",\"Google\",\"Transit\",\"Realtime\",\"VehicleDescriptor.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.VehicleDescriptor.id\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"VehicleDescriptor\"], baseName' = FName \"id\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.VehicleDescriptor.label\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"VehicleDescriptor\"], baseName' = FName \"label\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.VehicleDescriptor.license_plate\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"VehicleDescriptor\"], baseName' = FName \"license_plate\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [(FieldId {getFieldId = 1000},FieldId {getFieldId = 1999})], knownKeys = fromList [], storeUnknown = True, lazyFields = False, makeLenses = False, jsonInstances = False}"

instance P'.TextType VehicleDescriptor where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg VehicleDescriptor where
  textPut msg
   = do
       P'.tellT "id" (id msg)
       P'.tellT "label" (label msg)
       P'.tellT "license_plate" (license_plate msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'id, parse'label, parse'license_plate]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'id
         = P'.try
            (do
               v <- P'.getT "id"
               Prelude'.return (\ o -> o{id = v}))
        parse'label
         = P'.try
            (do
               v <- P'.getT "label"
               Prelude'.return (\ o -> o{label = v}))
        parse'license_plate
         = P'.try
            (do
               v <- P'.getT "license_plate"
               Prelude'.return (\ o -> o{license_plate = v}))