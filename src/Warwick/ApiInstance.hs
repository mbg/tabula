-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | For each University of Warwick web service, there are usually several
-- different instances (production, development, etc.) and we want our API
-- clients to work with all of them. To simplify this, we 
module Warwick.ApiInstance ( 
    ServiceName(..),
    ApiInstance(..),
    liveURL,
    devURL
) where 

-------------------------------------------------------------------------------

import Data.Aeson

import Servant.Client

import Warwick.Common

-------------------------------------------------------------------------------

-- | We use types to represent different web services which are identified by
-- a service name that usually corresponds to their subdomain.
class ServiceName service where
    -- `serviceName` retrieves the name of the service. E.g. "tabula" or
    -- "altexams".
    serviceName :: String

-- | Enumerates common Warwick API instances.
data ApiInstance service = Live | Dev | CustomInstance BaseUrl
    deriving (Eq, Show)

instance ToJSON (ApiInstance service) where 
    toJSON Live = String "live"
    toJSON Dev  = String "dev"
    toJSON (CustomInstance url) = 
        object [ "custom" .= url ]

instance FromJSON (ApiInstance service) where 
    parseJSON (String "live") = pure Live
    parseJSON (String "dev")  = pure Dev
    parseJSON val = flip (withObject "ApiInstance") val $ \obj -> 
        CustomInstance <$> obj .: "custom"

-- | `liveURL` represents the URL of the production instance of a service.
liveURL :: forall service . ServiceName service => BaseUrl
liveURL = BaseUrl Https (serviceName @service <> ".warwick.ac.uk") 443 ""

-- | `devURL` represents the URL of the development instance of a service.
devURL :: forall service . ServiceName service => BaseUrl
devURL = BaseUrl Https (serviceName @service <> "-dev.warwick.ac.uk") 443 ""

instance ServiceName service => HasBaseUrl (ApiInstance service) where
    getBaseUrl Live = liveURL @service
    getBaseUrl Dev = devURL @service
    getBaseUrl (CustomInstance url) = url

-------------------------------------------------------------------------------
