module CSV.Util where

import Database.Persist (PersistValue(..))

-- | TODO: better to use a class here?
--
maybeToPersist ::
  (a -> PersistValue)
  -> Maybe a
  -> PersistValue
maybeToPersist constr (Just a) = constr a
maybeToPersist _ Nothing = PersistNull
