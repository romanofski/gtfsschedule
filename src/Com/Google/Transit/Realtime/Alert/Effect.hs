{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Com.Google.Transit.Realtime.Alert.Effect (Effect(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data Effect = NO_SERVICE
            | REDUCED_SERVICE
            | SIGNIFICANT_DELAYS
            | DETOUR
            | ADDITIONAL_SERVICE
            | MODIFIED_SERVICE
            | OTHER_EFFECT
            | UNKNOWN_EFFECT
            | STOP_MOVED
            deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable Effect
 
instance Prelude'.Bounded Effect where
  minBound = NO_SERVICE
  maxBound = STOP_MOVED
 
instance P'.Default Effect where
  defaultValue = NO_SERVICE
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe Effect
toMaybe'Enum 1 = Prelude'.Just NO_SERVICE
toMaybe'Enum 2 = Prelude'.Just REDUCED_SERVICE
toMaybe'Enum 3 = Prelude'.Just SIGNIFICANT_DELAYS
toMaybe'Enum 4 = Prelude'.Just DETOUR
toMaybe'Enum 5 = Prelude'.Just ADDITIONAL_SERVICE
toMaybe'Enum 6 = Prelude'.Just MODIFIED_SERVICE
toMaybe'Enum 7 = Prelude'.Just OTHER_EFFECT
toMaybe'Enum 8 = Prelude'.Just UNKNOWN_EFFECT
toMaybe'Enum 9 = Prelude'.Just STOP_MOVED
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum Effect where
  fromEnum NO_SERVICE = 1
  fromEnum REDUCED_SERVICE = 2
  fromEnum SIGNIFICANT_DELAYS = 3
  fromEnum DETOUR = 4
  fromEnum ADDITIONAL_SERVICE = 5
  fromEnum MODIFIED_SERVICE = 6
  fromEnum OTHER_EFFECT = 7
  fromEnum UNKNOWN_EFFECT = 8
  fromEnum STOP_MOVED = 9
  toEnum
   = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Com.Google.Transit.Realtime.Alert.Effect") .
      toMaybe'Enum
  succ NO_SERVICE = REDUCED_SERVICE
  succ REDUCED_SERVICE = SIGNIFICANT_DELAYS
  succ SIGNIFICANT_DELAYS = DETOUR
  succ DETOUR = ADDITIONAL_SERVICE
  succ ADDITIONAL_SERVICE = MODIFIED_SERVICE
  succ MODIFIED_SERVICE = OTHER_EFFECT
  succ OTHER_EFFECT = UNKNOWN_EFFECT
  succ UNKNOWN_EFFECT = STOP_MOVED
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Com.Google.Transit.Realtime.Alert.Effect"
  pred REDUCED_SERVICE = NO_SERVICE
  pred SIGNIFICANT_DELAYS = REDUCED_SERVICE
  pred DETOUR = SIGNIFICANT_DELAYS
  pred ADDITIONAL_SERVICE = DETOUR
  pred MODIFIED_SERVICE = ADDITIONAL_SERVICE
  pred OTHER_EFFECT = MODIFIED_SERVICE
  pred UNKNOWN_EFFECT = OTHER_EFFECT
  pred STOP_MOVED = UNKNOWN_EFFECT
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Com.Google.Transit.Realtime.Alert.Effect"
 
instance P'.Wire Effect where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB Effect
 
instance P'.MessageAPI msg' (msg' -> Effect) Effect where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum Effect where
  reflectEnum
   = [(1, "NO_SERVICE", NO_SERVICE), (2, "REDUCED_SERVICE", REDUCED_SERVICE), (3, "SIGNIFICANT_DELAYS", SIGNIFICANT_DELAYS),
      (4, "DETOUR", DETOUR), (5, "ADDITIONAL_SERVICE", ADDITIONAL_SERVICE), (6, "MODIFIED_SERVICE", MODIFIED_SERVICE),
      (7, "OTHER_EFFECT", OTHER_EFFECT), (8, "UNKNOWN_EFFECT", UNKNOWN_EFFECT), (9, "STOP_MOVED", STOP_MOVED)]
  reflectEnumInfo _
   = P'.EnumInfo
      (P'.makePNF (P'.pack ".transit_realtime.Alert.Effect") [] ["Com", "Google", "Transit", "Realtime", "Alert"] "Effect")
      ["Com", "Google", "Transit", "Realtime", "Alert", "Effect.hs"]
      [(1, "NO_SERVICE"), (2, "REDUCED_SERVICE"), (3, "SIGNIFICANT_DELAYS"), (4, "DETOUR"), (5, "ADDITIONAL_SERVICE"),
       (6, "MODIFIED_SERVICE"), (7, "OTHER_EFFECT"), (8, "UNKNOWN_EFFECT"), (9, "STOP_MOVED")]
 
instance P'.TextType Effect where
  tellT = P'.tellShow
  getT = P'.getRead