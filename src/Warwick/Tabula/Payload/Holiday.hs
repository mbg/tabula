--------------------------------------------------------------------------------
-- Haskell bindings for the Tabula API                                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Tabula.Payload.Holiday where 

--------------------------------------------------------------------------------

import Data.Aeson
import Data.Time.Clock

import Warwick.Tabula.Types

--------------------------------------------------------------------------------

-- | Represents a holiday date.
newtype Holiday = Holiday { getHoliday :: TabulaDate }
    deriving Show

instance ToJSON Holiday where 
    toJSON (Holiday date) = toJSON date

instance FromJSON Holiday where 
    parseJSON v = Holiday <$> parseJSON v

instance HasPayload [Holiday] where
    payloadFieldName _ = "dates"

--------------------------------------------------------------------------------
