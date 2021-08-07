{-# LANGUAGE TemplateHaskell, BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses,
 OverloadedStrings #-}
{-# OPTIONS_GHC  -w #-}
module GTFS.Realtime.Internal.Com.Google.Transit.Realtime.Alert
       (Alert(..), active_period, informed_entity, cause, effect, url, header_text, description_text) where
import Prelude ((+), (/), (++), (.), (==), (<=), (&&))
import qualified Prelude as Prelude'
import qualified Data.List as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Control.Lens.TH
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.Alert.Cause as Com.Google.Transit.Realtime.Alert (Cause)
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.Alert.Effect as Com.Google.Transit.Realtime.Alert (Effect)
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.EntitySelector as Com.Google.Transit.Realtime (EntitySelector)
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TimeRange as Com.Google.Transit.Realtime (TimeRange)
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TranslatedString as Com.Google.Transit.Realtime
       (TranslatedString)

data Alert = Alert{_active_period :: !(P'.Seq Com.Google.Transit.Realtime.TimeRange),
                   _informed_entity :: !(P'.Seq Com.Google.Transit.Realtime.EntitySelector),
                   _cause :: !(P'.Maybe Com.Google.Transit.Realtime.Alert.Cause),
                   _effect :: !(P'.Maybe Com.Google.Transit.Realtime.Alert.Effect),
                   _url :: !(P'.Maybe Com.Google.Transit.Realtime.TranslatedString),
                   _header_text :: !(P'.Maybe Com.Google.Transit.Realtime.TranslatedString),
                   _description_text :: !(P'.Maybe Com.Google.Transit.Realtime.TranslatedString), _ext'field :: !(P'.ExtField),
                   _unknown'field :: !(P'.UnknownField)}
             deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

Control.Lens.TH.makeLenses ''Alert

instance P'.ExtendMessage Alert where
  getExtField = _ext'field
  putExtField e'f msg = msg{_ext'field = e'f}
  validExtRanges msg = P'.extRanges (P'.reflectDescriptorInfo msg)

instance P'.UnknownMessage Alert where
  getUnknownField = _unknown'field
  putUnknownField u'f msg = msg{_unknown'field = u'f}

instance P'.Mergeable Alert where
  mergeAppend (Alert x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9) (Alert y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8 y'9)
   = let !z'1 = P'.mergeAppend x'1 y'1
         !z'2 = P'.mergeAppend x'2 y'2
         !z'3 = P'.mergeAppend x'3 y'3
         !z'4 = P'.mergeAppend x'4 y'4
         !z'5 = P'.mergeAppend x'5 y'5
         !z'6 = P'.mergeAppend x'6 y'6
         !z'7 = P'.mergeAppend x'7 y'7
         !z'8 = P'.mergeAppend x'8 y'8
         !z'9 = P'.mergeAppend x'9 y'9
      in Alert z'1 z'2 z'3 z'4 z'5 z'6 z'7 z'8 z'9

instance P'.Default Alert where
  defaultValue
   = Alert P'.defaultValue P'.defaultValue (Prelude'.Just (Prelude'.read "UNKNOWN_CAUSE"))
      (Prelude'.Just (Prelude'.read "UNKNOWN_EFFECT"))
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue

instance P'.Wire Alert where
  wireSize ft' self'@(Alert x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeRep 1 11 x'1 + P'.wireSizeRep 1 11 x'2 + P'.wireSizeOpt 1 14 x'3 + P'.wireSizeOpt 1 14 x'4 +
             P'.wireSizeOpt 1 11 x'5
             + P'.wireSizeOpt 1 11 x'6
             + P'.wireSizeOpt 1 11 x'7
             + P'.wireSizeExtField x'8
             + P'.wireSizeUnknownField x'9)
  wirePutWithSize ft' self'@(Alert x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9)
   = case ft' of
       10 -> put'Fields
       11 -> put'FieldsSized
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = P'.sequencePutWithSize
            [P'.wirePutRepWithSize 10 11 x'1, P'.wirePutRepWithSize 42 11 x'2, P'.wirePutOptWithSize 48 14 x'3,
             P'.wirePutOptWithSize 56 14 x'4, P'.wirePutOptWithSize 66 11 x'5, P'.wirePutOptWithSize 82 11 x'6,
             P'.wirePutOptWithSize 90 11 x'7, P'.wirePutExtFieldWithSize x'8, P'.wirePutUnknownFieldWithSize x'9]
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
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{_active_period = P'.append (_active_period old'Self) new'Field})
                    (P'.wireGet 11)
             42 -> Prelude'.fmap (\ !new'Field -> old'Self{_informed_entity = P'.append (_informed_entity old'Self) new'Field})
                    (P'.wireGet 11)
             48 -> Prelude'.fmap (\ !new'Field -> old'Self{_cause = Prelude'.Just new'Field}) (P'.wireGet 14)
             56 -> Prelude'.fmap (\ !new'Field -> old'Self{_effect = Prelude'.Just new'Field}) (P'.wireGet 14)
             66 -> Prelude'.fmap (\ !new'Field -> old'Self{_url = P'.mergeAppend (_url old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             82 -> Prelude'.fmap
                    (\ !new'Field -> old'Self{_header_text = P'.mergeAppend (_header_text old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             90 -> Prelude'.fmap
                    (\ !new'Field ->
                      old'Self{_description_text = P'.mergeAppend (_description_text old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in
                   if Prelude'.or [1000 <= field'Number && field'Number <= 1999] then
                    P'.loadExtension field'Number wire'Type old'Self else P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> Alert) Alert where
  getVal m' f' = f' m'

instance P'.GPB Alert

instance P'.ReflectDescriptor Alert where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 42, 48, 56, 66, 82, 90])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".transit_realtime.Alert\", haskellPrefix = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\"], baseName = MName \"Alert\"}, descFilePath = [\"GTFS\",\"Realtime\",\"Internal\",\"Com\",\"Google\",\"Transit\",\"Realtime\",\"Alert.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.Alert.active_period\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"Alert\"], baseName' = FName \"active_period\", baseNamePrefix' = \"_\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".transit_realtime.TimeRange\", haskellPrefix = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\"], baseName = MName \"TimeRange\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.Alert.informed_entity\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"Alert\"], baseName' = FName \"informed_entity\", baseNamePrefix' = \"_\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 42}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".transit_realtime.EntitySelector\", haskellPrefix = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\"], baseName = MName \"EntitySelector\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.Alert.cause\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"Alert\"], baseName' = FName \"cause\", baseNamePrefix' = \"_\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 48}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".transit_realtime.Alert.Cause\", haskellPrefix = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"Alert\"], baseName = MName \"Cause\"}), hsRawDefault = Just \"UNKNOWN_CAUSE\", hsDefault = Just (HsDef'Enum \"UNKNOWN_CAUSE\")},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.Alert.effect\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"Alert\"], baseName' = FName \"effect\", baseNamePrefix' = \"_\"}, fieldNumber = FieldId {getFieldId = 7}, wireTag = WireTag {getWireTag = 56}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".transit_realtime.Alert.Effect\", haskellPrefix = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"Alert\"], baseName = MName \"Effect\"}), hsRawDefault = Just \"UNKNOWN_EFFECT\", hsDefault = Just (HsDef'Enum \"UNKNOWN_EFFECT\")},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.Alert.url\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"Alert\"], baseName' = FName \"url\", baseNamePrefix' = \"_\"}, fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 66}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".transit_realtime.TranslatedString\", haskellPrefix = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\"], baseName = MName \"TranslatedString\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.Alert.header_text\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"Alert\"], baseName' = FName \"header_text\", baseNamePrefix' = \"_\"}, fieldNumber = FieldId {getFieldId = 10}, wireTag = WireTag {getWireTag = 82}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".transit_realtime.TranslatedString\", haskellPrefix = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\"], baseName = MName \"TranslatedString\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.Alert.description_text\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"Alert\"], baseName' = FName \"description_text\", baseNamePrefix' = \"_\"}, fieldNumber = FieldId {getFieldId = 11}, wireTag = WireTag {getWireTag = 90}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".transit_realtime.TranslatedString\", haskellPrefix = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\"], baseName = MName \"TranslatedString\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [(FieldId {getFieldId = 1000},FieldId {getFieldId = 1999})], knownKeys = fromList [], storeUnknown = True, lazyFields = False, makeLenses = True, jsonInstances = False}"

instance P'.TextType Alert where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg Alert where
  textPut msg
   = do
       P'.tellT "active_period" (_active_period msg)
       P'.tellT "informed_entity" (_informed_entity msg)
       P'.tellT "cause" (_cause msg)
       P'.tellT "effect" (_effect msg)
       P'.tellT "url" (_url msg)
       P'.tellT "header_text" (_header_text msg)
       P'.tellT "description_text" (_description_text msg)
  textGet
   = do
       mods <- P'.sepEndBy
                (P'.choice
                  [parse'_active_period, parse'_informed_entity, parse'_cause, parse'_effect, parse'_url, parse'_header_text,
                   parse'_description_text])
                P'.spaces
       Prelude'.return (Prelude'.foldl' (\ v f -> f v) P'.defaultValue mods)
    where
        parse'_active_period
         = Prelude'.fmap (\ v o -> o{_active_period = P'.append (_active_period o) v}) (P'.try (P'.getT "active_period"))
        parse'_informed_entity
         = Prelude'.fmap (\ v o -> o{_informed_entity = P'.append (_informed_entity o) v}) (P'.try (P'.getT "informed_entity"))
        parse'_cause = Prelude'.fmap (\ v o -> o{_cause = v}) (P'.try (P'.getT "cause"))
        parse'_effect = Prelude'.fmap (\ v o -> o{_effect = v}) (P'.try (P'.getT "effect"))
        parse'_url = Prelude'.fmap (\ v o -> o{_url = v}) (P'.try (P'.getT "url"))
        parse'_header_text = Prelude'.fmap (\ v o -> o{_header_text = v}) (P'.try (P'.getT "header_text"))
        parse'_description_text = Prelude'.fmap (\ v o -> o{_description_text = v}) (P'.try (P'.getT "description_text"))