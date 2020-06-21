-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.Campus.Room (
    Room(..)
) where 

-------------------------------------------------------------------------------

import Data.Aeson 
import Data.Text

-------------------------------------------------------------------------------

-- | Represents a room result returned from the Campus API.
data Room = Room {
    -- | The unique ID of the room.
    roomId :: Int,
    -- | The name of the room.
    roomName :: Text,
    -- | No idea.
    roomW2Id :: Int,
    -- | No idea.
    roomBf :: Int,
    -- | The name of the building in which the room is located.
    roomBuilding :: Text,
    -- | The name of the floor on which the room is located.
    roomFloor :: Text,
    -- | The room category.
    roomCategory :: Maybe Text,
    -- | The GPS latitude of the room.
    roomGpsLat :: Double,
    -- | The GPS longitude of the room.
    roomGpsLon :: Double
} deriving (Eq, Show)

instance FromJSON Room where 
    parseJSON = withObject "Room" $ \obj ->
        Room <$> obj .: "id"
             <*> obj .: "value"
             <*> obj .: "w2gid"
             <*> obj .: "bf"
             <*> obj .: "building"
             <*> obj .: "floor"
             <*> obj .: "category"
             <*> obj .: "gpsLat"
             <*> obj .: "gpsLon"

-------------------------------------------------------------------------------
