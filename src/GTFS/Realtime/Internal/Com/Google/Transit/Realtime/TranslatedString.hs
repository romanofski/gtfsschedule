{-# LANGUAGE TemplateHaskell, BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses,
 OverloadedStrings #-}
{-# OPTIONS_GHC  -w #-}
module GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TranslatedString (TranslatedString(..), translation) where
import Prelude ((+), (/), (++), (.), (==), (<=), (&&))
import qualified Prelude as Prelude'
import qualified Data.List as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Control.Lens.TH
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TranslatedString.Translation
       as Com.Google.Transit.Realtime.TranslatedString (Translation)

data TranslatedString = TranslatedString{_translation :: !(P'.Seq Com.Google.Transit.Realtime.TranslatedString.Translation),
                                         _ext'field :: !(P'.ExtField), _unknown'field :: !(P'.UnknownField)}
                        deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

Control.Lens.TH.makeLenses ''TranslatedString

instance P'.ExtendMessage TranslatedString where
  getExtField = _ext'field
  putExtField e'f msg = msg{_ext'field = e'f}
  validExtRanges msg = P'.extRanges (P'.reflectDescriptorInfo msg)

instance P'.UnknownMessage TranslatedString where
  getUnknownField = _unknown'field
  putUnknownField u'f msg = msg{_unknown'field = u'f}

instance P'.Mergeable TranslatedString where
  mergeAppend (TranslatedString x'1 x'2 x'3) (TranslatedString y'1 y'2 y'3)
   = let !z'1 = P'.mergeAppend x'1 y'1
         !z'2 = P'.mergeAppend x'2 y'2
         !z'3 = P'.mergeAppend x'3 y'3
      in TranslatedString z'1 z'2 z'3

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
  wirePutWithSize ft' self'@(TranslatedString x'1 x'2 x'3)
   = case ft' of
       10 -> put'Fields
       11 -> put'FieldsSized
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = P'.sequencePutWithSize
            [P'.wirePutRepWithSize 10 11 x'1, P'.wirePutExtFieldWithSize x'2, P'.wirePutUnknownFieldWithSize x'3]
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
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{_translation = P'.append (_translation old'Self) new'Field})
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
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".transit_realtime.TranslatedString\", haskellPrefix = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\"], baseName = MName \"TranslatedString\"}, descFilePath = [\"GTFS\",\"Realtime\",\"Internal\",\"Com\",\"Google\",\"Transit\",\"Realtime\",\"TranslatedString.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.TranslatedString.translation\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"TranslatedString\"], baseName' = FName \"translation\", baseNamePrefix' = \"_\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".transit_realtime.TranslatedString.Translation\", haskellPrefix = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"TranslatedString\"], baseName = MName \"Translation\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [(FieldId {getFieldId = 1000},FieldId {getFieldId = 1999})], knownKeys = fromList [], storeUnknown = True, lazyFields = False, makeLenses = True, jsonInstances = False}"

instance P'.TextType TranslatedString where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg TranslatedString where
  textPut msg
   = do
       P'.tellT "translation" (_translation msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'_translation]) P'.spaces
       Prelude'.return (Prelude'.foldl' (\ v f -> f v) P'.defaultValue mods)
    where
        parse'_translation
         = Prelude'.fmap (\ v o -> o{_translation = P'.append (_translation o) v}) (P'.try (P'.getT "translation"))