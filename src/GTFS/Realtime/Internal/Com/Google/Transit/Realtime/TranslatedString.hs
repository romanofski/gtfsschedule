{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TranslatedString (TranslatedString(..)) where
import Prelude ((+), (/), (==), (<=), (&&))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TranslatedString.Translation
       as Com.Google.Transit.Realtime.TranslatedString (Translation)

data TranslatedString = TranslatedString{translation :: !(P'.Seq Com.Google.Transit.Realtime.TranslatedString.Translation),
                                         ext'field :: !(P'.ExtField), unknown'field :: !(P'.UnknownField)}
                      deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)

instance P'.ExtendMessage TranslatedString where
  getExtField = ext'field
  putExtField e'f msg = msg{ext'field = e'f}
  validExtRanges msg = P'.extRanges (P'.reflectDescriptorInfo msg)

instance P'.UnknownMessage TranslatedString where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}

instance P'.Mergeable TranslatedString where
  mergeAppend (TranslatedString x'1 x'2 x'3) (TranslatedString y'1 y'2 y'3)
   = TranslatedString (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3)

instance P'.Default TranslatedString where
  defaultValue = TranslatedString P'.defaultValue P'.defaultValue P'.defaultValue

instance P'.Wire TranslatedString where
  wireSize ft' self'@(TranslatedString x'1 x'2 x'3)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeRep 1 11 x'1 + P'.wireSizeExtField x'2 + P'.wireSizeUnknownField x'3)
  wirePut ft' self'@(TranslatedString x'1 x'2 x'3)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutRep 10 11 x'1
             P'.wirePutExtField x'2
             P'.wirePutUnknownField x'3
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith (P'.catch'Unknown update'Self)
       11 -> P'.getMessageWith (P'.catch'Unknown update'Self)
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{translation = P'.append (translation old'Self) new'Field})
                    (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in
                   if Prelude'.or [1000 <= field'Number && field'Number <= 1999] then
                    P'.loadExtension field'Number wire'Type old'Self else P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> TranslatedString) TranslatedString where
  getVal m' f' = f' m'

instance P'.GPB TranslatedString

instance P'.ReflectDescriptor TranslatedString where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".transit_realtime.TranslatedString\", haskellPrefix = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\"], baseName = MName \"TranslatedString\"}, descFilePath = [\"GTFS\",\"Realtime\",\"Internal\",\"Com\",\"Google\",\"Transit\",\"Realtime\",\"TranslatedString.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.TranslatedString.translation\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"TranslatedString\"], baseName' = FName \"translation\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".transit_realtime.TranslatedString.Translation\", haskellPrefix = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"TranslatedString\"], baseName = MName \"Translation\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [(FieldId {getFieldId = 1000},FieldId {getFieldId = 1999})], knownKeys = fromList [], storeUnknown = True, lazyFields = False, makeLenses = False}"

instance P'.TextType TranslatedString where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg TranslatedString where
  textPut msg
   = do
       P'.tellT "translation" (translation msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'translation]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'translation
         = P'.try
            (do
               v <- P'.getT "translation"
               Prelude'.return (\ o -> o{translation = P'.append (translation o) v}))