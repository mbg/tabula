--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

-- | This module contains shared types and functions related to configuring how
-- the API client should access the various APIs.
module Warwick.Config where 

--------------------------------------------------------------------------------

import Data.Aeson 
import Data.ByteString.Lazy as BS (readFile)
import Data.Text

--------------------------------------------------------------------------------

-- | Represents configurations for accessing the APIs.
data APIConfig = APIConfig {
    apiUsername :: Text,
    apiPassword :: Text
} 

instance FromJSON APIConfig where
    parseJSON = withObject "APIConfig" $ \v ->
        APIConfig <$> v .: "username"
                  <*> v .: "password"

-- | `readAPIConfig` @fp@ reads the configuration for the API client
-- from a file located at @fp@.
readAPIConfig :: FilePath -> IO (Maybe APIConfig)
readAPIConfig fp = decode <$> BS.readFile fp

--------------------------------------------------------------------------------