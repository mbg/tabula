--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Peoplesearch.Response where 

--------------------------------------------------------------------------------

import Data.Aeson

--------------------------------------------------------------------------------

-- | Represents responses from the peoplesearch API.
data PeoplesearchResponse a = PSResponse {
    -- | The total number of search results.
    psrTotal :: Int,
    -- | The number of results returned.
    psrReturned :: Int, 
    -- | The results.
    psrData :: [a]
} deriving Show

instance FromJSON a => FromJSON (PeoplesearchResponse a) where
    parseJSON = withObject "PeoplesearchResponse" $ \obj ->
        PSResponse <$> obj .: "total"
                   <*> obj .: "totalReturned"
                   <*> obj .: "data"

--------------------------------------------------------------------------------