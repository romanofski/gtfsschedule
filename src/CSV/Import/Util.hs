module CSV.Import.Util where

import Database.Persist (PersistValue(..))

-- | Convert a maybe value to a either PersistNull if Nothing or a default PersistValue
-- TODO: better to use a class here?
--
maybeToPersist ::
  (a -> PersistValue)  -- ^ PersistValue constructor
  -> Maybe a  -- ^ a Maybe
  -> PersistValue
maybeToPersist constr (Just a) = constr a
maybeToPersist _ Nothing = PersistNull
