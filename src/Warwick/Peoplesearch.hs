--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Peoplesearch (
    module Warwick.Config,
    module Warwick.Common,

    PeoplesearchInstance(..),

    searchProfiles,
    lookupProfile
) where 

--------------------------------------------------------------------------------

import Control.Monad.Trans

import Data.Aeson
import Data.Text

import Servant.Client

import Warwick.Config
import Warwick.Common
import qualified Warwick.Peoplesearch.Response as PS
import qualified Warwick.Peoplesearch.Profile as PS
import qualified Warwick.Peoplesearch.API as PS

--------------------------------------------------------------------------------

-- | Enumerates Peoplesearch instances.
data PeoplesearchInstance = Live | CustomInstance BaseUrl

instance ToJSON PeoplesearchInstance where 
    toJSON Live    = String "live"
    toJSON (CustomInstance url) = 
        object [ "custom" .= url ]

instance FromJSON PeoplesearchInstance where 
    parseJSON (String "live")    = pure Live 
    parseJSON val = flip (withObject "PeoplesearchInstance") val $ \obj -> 
        CustomInstance <$> obj .: "custom"

-- | The URL to the Peoplesearch API.
liveURL :: BaseUrl
liveURL = BaseUrl Https "peoplesearch.warwick.ac.uk" 443 "/api"

instance HasBaseUrl PeoplesearchInstance where
    getBaseUrl Live                 = liveURL
    getBaseUrl (CustomInstance url) = url

-------------------------------------------------------------------------------

-- | `searchProfiles` @query@ searches profiles for @query@.
searchProfiles ::
    Text -> Warwick (PS.PeoplesearchResponse PS.Profile)
searchProfiles query = do
    authData <- getAuthData
    lift $ lift $ PS.searchProfiles authData (Just query)

-- | `lookupProfile` @universityID@ looks up profiles for the user identified
-- by @universityID@.
lookupProfile ::
    Text -> Warwick (PS.PeoplesearchResponse PS.Profile)
lookupProfile uid = do
    authData <- getAuthData
    lift $ lift $ PS.lookupProfile authData [uid <> ".json"]

--------------------------------------------------------------------------------
