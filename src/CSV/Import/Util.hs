{-
Copyright (C) - 2017 Róman Joost <roman@bromeco.de>

This file is part of gtfsschedule.

gtfsschedule is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

gtfsschedule is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with gtfsschedule.  If not, see <http://www.gnu.org/licenses/>.
-}
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
