{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TranslatedString.Translation (Translation(..)) where
import Prelude ((+), (/), (++), (.), (==), (<=), (&&))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data Translation = Translation{text :: !(P'.Utf8), language :: !(P'.Maybe P'.Utf8), ext'field :: !(P'.ExtField),
                               unknown'field :: !(P'.UnknownField)}
                   deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.ExtendMessage Translation where
  getExtField = ext'field
  putExtField e'f msg = msg{ext'field = e'f}
  validExtRanges msg = P'.extRanges (P'.reflectDescriptorInfo msg)

instance P'.UnknownMessage Translation where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}

instance P'.Mergeable Translation where
  mergeAppend (Translation x'1 x'2 x'3 x'4) (Translation y'1 y'2 y'3 y'4)
   = Translation (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)

instance P'.Default Translation where
  defaultValue = Translation P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue

instance P'.Wire Translation where
  wireSize ft' self'@(Translation x'1 x'2 x'3 x'4)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 9 x'1 + P'.wireSizeOpt 1 9 x'2 + P'.wireSizeExtField x'3 + P'.wireSizeUnknownField x'4)
  wirePutWithSize ft' self'@(Translation x'1 x'2 x'3 x'4)
   = case ft' of
       10 -> put'Fields
       11 -> put'FieldsSized
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = P'.sequencePutWithSize
            [P'.wirePutReqWithSize 10 9 x'1, P'.wirePutOptWithSize 18 9 x'2, P'.wirePutExtFieldWithSize x'3,
             P'.wirePutUnknownFieldWithSize x'4]
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
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{text = new'Field}) (P'.wireGet 9)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{language = Prelude'.Just new'Field}) (P'.wireGet 9)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in
                   if Prelude'.or [1000 <= field'Number && field'Number <= 1999] then
                    P'.loadExtension field'Number wire'Type old'Self else P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> Translation) Translation where
  getVal m' f' = f' m'

instance P'.GPB Translation

instance P'.ReflectDescriptor Translation where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [10]) (P'.fromDistinctAscList [10, 18])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".transit_realtime.TranslatedString.Translation\", haskellPrefix = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"TranslatedString\"], baseName = MName \"Translation\"}, descFilePath = [\"GTFS\",\"Realtime\",\"Internal\",\"Com\",\"Google\",\"Transit\",\"Realtime\",\"TranslatedString\",\"Translation.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.TranslatedString.Translation.text\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"TranslatedString\",MName \"Translation\"], baseName' = FName \"text\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.TranslatedString.Translation.language\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"TranslatedString\",MName \"Translation\"], baseName' = FName \"language\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [(FieldId {getFieldId = 1000},FieldId {getFieldId = 1999})], knownKeys = fromList [], storeUnknown = True, lazyFields = False, makeLenses = False, jsonInstances = False}"

instance P'.TextType Translation where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg Translation where
  textPut msg
   = do
       P'.tellT "text" (text msg)
       P'.tellT "language" (language msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'text, parse'language]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'text
         = P'.try
            (do
               v <- P'.getT "text"
               Prelude'.return (\ o -> o{text = v}))
        parse'language
         = P'.try
            (do
               v <- P'.getT "language"
               Prelude'.return (\ o -> o{language = v}))