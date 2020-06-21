-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.Campus (
    module Warwick.Campus.Room,

    CampusInstance(..),

    Campus,
    withCampus,

    listRooms
) where 

-------------------------------------------------------------------------------

import Control.Monad.Except
import Control.Monad.Reader

import Data.Aeson
import Data.Text

import Network.HTTP.Conduit

import Servant.Client

import Warwick.Common
import Warwick.Campus.Room
import qualified Warwick.Campus.API as API

-------------------------------------------------------------------------------

-- | Enumerates Campus API instances.
data CampusInstance = Live | CustomInstance BaseUrl
    deriving (Eq, Show)

instance ToJSON CampusInstance where 
    toJSON Live    = String "live"
    toJSON (CustomInstance url) = 
        object [ "custom" .= url ]

instance FromJSON CampusInstance where 
    parseJSON (String "live")    = pure Live 
    parseJSON val = flip (withObject "CampusInstance") val $ \obj -> 
        CustomInstance <$> obj .: "custom"

-- | The URL to the Campus API.
liveURL :: BaseUrl
liveURL = BaseUrl Https "campus-cms.warwick.ac.uk" 443 "/"

instance HasBaseUrl CampusInstance where
    getBaseUrl Live                 = liveURL
    getBaseUrl (CustomInstance url) = url

-------------------------------------------------------------------------------

type Campus = ReaderT Text (ExceptT APIError ClientM)

-- | 'withCampus' @instance token computation@ performs @computation@ 
-- which uses the Campus API by connecting to @instance@ using @token@.
withCampus :: CampusInstance -> Text -> Campus a -> IO (Either APIError a)
withCampus inst token m = do
    manager <- newManager tlsManagerSettings

    let url  = getBaseUrl inst
    let env  = ClientEnv manager url

    r <- runClientM (runExceptT (runReaderT m token)) (env Nothing)

    case r of
        Left serr -> pure $ Left $ TransportError serr
        Right res -> pure res

-------------------------------------------------------------------------------

-- | 'listRooms' @limit query@ searches for rooms with names matching 
-- @query@. At most @limit@-many results are returned.
listRooms :: Int -> Text -> Campus [Room]
listRooms limit query = do 
    token <- ask 
    lift $ lift $ API.listRooms 
        (Just $ "Token " <> token) (Just 2) (Just limit) (Just query) (Just False)

-------------------------------------------------------------------------------
