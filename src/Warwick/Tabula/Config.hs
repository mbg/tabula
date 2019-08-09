--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Tabula.Config (
    TabulaInstance(..),
    liveURL,
    sandboxURL,
    urlForInstance
) where

--------------------------------------------------------------------------------

import GHC.Generics

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.Text

import Servant.Client

-------------------------------------------------------------------------------

-- | Enumerates Tabula instances.
data TabulaInstance = Sandbox | Live | CustomInstance BaseUrl

instance ToJSON TabulaInstance where 
    toJSON Sandbox = String "sandbox"
    toJSON Live    = String "live"
    toJSON (CustomInstance url) = 
        object [ "custom" .= url ]

instance FromJSON TabulaInstance where 
    parseJSON (String "sandbox") = pure Sandbox 
    parseJSON (String "live")    = pure Live 
    parseJSON val = flip (withObject "TabulaInstance") val $ \obj -> 
        CustomInstance <$> obj .: "custom"

-- | The URL to the Tabula API.
liveURL :: BaseUrl
liveURL = BaseUrl Https "tabula.warwick.ac.uk" 443 "/api/v1"

-- | The URL to the Tabula Sandbox API.
sandboxURL :: BaseUrl
sandboxURL = BaseUrl Https "tabula-sandbox.warwick.ac.uk" 443 "/api/v1"

-- | Determines the location of a given Tabula instance.
urlForInstance :: TabulaInstance -> BaseUrl
urlForInstance Sandbox              = sandboxURL
urlForInstance Live                 = liveURL
urlForInstance (CustomInstance url) = url

--------------------------------------------------------------------------------
