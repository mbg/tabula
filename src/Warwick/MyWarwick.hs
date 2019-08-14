--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.MyWarwick (
    module Warwick.Config,
    module Warwick.Common,

    MyWarwickInstance(..),

    postAlert,
    postActivity
) where 

--------------------------------------------------------------------------------

import Control.Monad.Trans

import Data.Aeson
import Data.Text

import Servant.Client

import Warwick.Config
import Warwick.Common
import qualified Warwick.MyWarwick.Response as My
import qualified Warwick.MyWarwick.StreamItem as My
import qualified Warwick.MyWarwick.API as My

--------------------------------------------------------------------------------

-- | Enumerates MyWarwick instances.
data MyWarwickInstance = Live | CustomInstance BaseUrl

instance ToJSON MyWarwickInstance where 
    toJSON Live    = String "live"
    toJSON (CustomInstance url) = 
        object [ "custom" .= url ]

instance FromJSON MyWarwickInstance where 
    parseJSON (String "live")    = pure Live 
    parseJSON val = flip (withObject "MyWarwickInstance") val $ \obj -> 
        CustomInstance <$> obj .: "custom"

-- | The URL to the MyWarwick API.
liveURL :: BaseUrl
liveURL = BaseUrl Https "my.warwick.ac.uk" 443 "/api"

instance HasBaseUrl MyWarwickInstance where
    getBaseUrl Live                 = liveURL
    getBaseUrl (CustomInstance url) = url

-------------------------------------------------------------------------------

-- | `postAlert` @providerID item@ posts @item@ as an alert using @providerID@.
postAlert ::
    Text -> My.StreamItem -> Warwick My.MyWarwickResponse
postAlert providerID item = do
    authData <- getAuthData
    lift $ lift $ My.postAlert authData providerID item

-- | `postActivity` @providerID item@ posts @item@ as an activity using 
-- @providerID@.
postActivity ::
    Text -> My.StreamItem -> Warwick My.MyWarwickResponse
postActivity providerID item = do
    authData <- getAuthData
    lift $ lift $ My.postActivity authData providerID item

--------------------------------------------------------------------------------
