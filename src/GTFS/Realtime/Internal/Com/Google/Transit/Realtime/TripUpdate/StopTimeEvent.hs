{-# LANGUAGE TemplateHaskell, BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses,
 OverloadedStrings #-}
{-# OPTIONS_GHC  -w #-}
module GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TripUpdate.StopTimeEvent (StopTimeEvent(..), delay, time, uncertainty)
       where
import Prelude ((+), (/), (++), (.), (==), (<=), (&&))
import qualified Prelude as Prelude'
import qualified Data.List as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Control.Lens.TH

data StopTimeEvent = StopTimeEvent{_delay :: !(P'.Maybe P'.Int32), _time :: !(P'.Maybe P'.Int64),
                                   _uncertainty :: !(P'.Maybe P'.Int32), _ext'field :: !(P'.ExtField),
                                   _unknown'field :: !(P'.UnknownField)}
                     deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

Control.Lens.TH.makeLenses ''StopTimeEvent

instance P'.ExtendMessage StopTimeEvent where
  getExtField = _ext'field
  putExtField e'f msg = msg{_ext'field = e'f}
  validExtRanges msg = P'.extRanges (P'.reflectDescriptorInfo msg)

instance P'.UnknownMessage StopTimeEvent where
  getUnknownField = _unknown'field
  putUnknownField u'f msg = msg{_unknown'field = u'f}

instance P'.Mergeable StopTimeEvent where
  mergeAppend (StopTimeEvent x'1 x'2 x'3 x'4 x'5) (StopTimeEvent y'1 y'2 y'3 y'4 y'5)
   = let !z'1 = P'.mergeAppend x'1 y'1
         !z'2 = P'.mergeAppend x'2 y'2
         !z'3 = P'.mergeAppend x'3 y'3
         !z'4 = P'.mergeAppend x'4 y'4
         !z'5 = P'.mergeAppend x'5 y'5
      in StopTimeEvent z'1 z'2 z'3 z'4 z'5

instance P'.Default StopTimeEvent where
  defaultValue = StopTimeEvent P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue

instance P'.Wire StopTimeEvent where
  wireSize ft' self'@(StopTimeEvent x'1 x'2 x'3 x'4 x'5)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeOpt 1 5 x'1 + P'.wireSizeOpt 1 3 x'2 + P'.wireSizeOpt 1 5 x'3 + P'.wireSizeExtField x'4 +
             P'.wireSizeUnknownField x'5)
  wirePutWithSize ft' self'@(StopTimeEvent x'1 x'2 x'3 x'4 x'5)
   = case ft' of
       10 -> put'Fields
       11 -> put'FieldsSized
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = P'.sequencePutWithSize
            [P'.wirePutOptWithSize 8 5 x'1, P'.wirePutOptWithSize 16 3 x'2, P'.wirePutOptWithSize 24 5 x'3,
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
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{_delay = Prelude'.Just new'Field}) (P'.wireGet 5)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{_time = Prelude'.Just new'Field}) (P'.wireGet 3)
             24 -> Prelude'.fmap (\ !new'Field -> old'Self{_uncertainty = Prelude'.Just new'Field}) (P'.wireGet 5)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in
                   if Prelude'.or [1000 <= field'Number && field'Number <= 1999] then
                    P'.loadExtension field'Number wire'Type old'Self else P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> StopTimeEvent) StopTimeEvent where
  getVal m' f' = f' m'

instance P'.GPB StopTimeEvent

instance P'.ReflectDescriptor StopTimeEvent where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8, 16, 24])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".transit_realtime.TripUpdate.StopTimeEvent\", haskellPrefix = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"TripUpdate\"], baseName = MName \"StopTimeEvent\"}, descFilePath = [\"GTFS\",\"Realtime\",\"Internal\",\"Com\",\"Google\",\"Transit\",\"Realtime\",\"TripUpdate\",\"StopTimeEvent.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.TripUpdate.StopTimeEvent.delay\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"TripUpdate\",MName \"StopTimeEvent\"], baseName' = FName \"delay\", baseNamePrefix' = \"_\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.TripUpdate.StopTimeEvent.time\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"TripUpdate\",MName \"StopTimeEvent\"], baseName' = FName \"time\", baseNamePrefix' = \"_\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".transit_realtime.TripUpdate.StopTimeEvent.uncertainty\", haskellPrefix' = [MName \"GTFS\",MName \"Realtime\",MName \"Internal\"], parentModule' = [MName \"Com\",MName \"Google\",MName \"Transit\",MName \"Realtime\",MName \"TripUpdate\",MName \"StopTimeEvent\"], baseName' = FName \"uncertainty\", baseNamePrefix' = \"_\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [(FieldId {getFieldId = 1000},FieldId {getFieldId = 1999})], knownKeys = fromList [], storeUnknown = True, lazyFields = False, makeLenses = True, jsonInstances = False}"

instance P'.TextType StopTimeEvent where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg StopTimeEvent where
  textPut msg
   = do
       P'.tellT "delay" (_delay msg)
       P'.tellT "time" (_time msg)
       P'.tellT "uncertainty" (_uncertainty msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'_delay, parse'_time, parse'_uncertainty]) P'.spaces
       Prelude'.return (Prelude'.foldl' (\ v f -> f v) P'.defaultValue mods)
    where
        parse'_delay = Prelude'.fmap (\ v o -> o{_delay = v}) (P'.try (P'.getT "delay"))
        parse'_time = Prelude'.fmap (\ v o -> o{_time = v}) (P'.try (P'.getT "time"))
        parse'_uncertainty = Prelude'.fmap (\ v o -> o{_uncertainty = v}) (P'.try (P'.getT "uncertainty"))