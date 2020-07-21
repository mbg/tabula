-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.Tabula.Payload.Location (
    Location(..)
) where 

-------------------------------------------------------------------------------

import Data.Aeson
import Data.Text

-------------------------------------------------------------------------------

-- | Represents campus locations.
data Location = Location {
    -- | The name of the location.
    locationName :: Text,
    -- | The unique ID of the location.
    locationId :: Text
} deriving (Eq, Show)

instance FromJSON Location where 
    parseJSON = withObject "Location" $ \obj ->
        Location <$> obj .: "name" 
                 <*> obj .: "locationId"

-------------------------------------------------------------------------------
