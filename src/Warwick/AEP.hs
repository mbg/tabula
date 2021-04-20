-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.AEP (
    module Warwick.Common,
    module Warwick.AEP.FileUpload,

    AEPInstance(..),

    AEP,
    withAEP,

    uploadFile
) where 

--------------------------------------------------------------------------------

import Control.Monad.Except
import Control.Monad.Reader

import Data.Aeson
import Data.Text
import Data.Text.Encoding
import Data.UUID.Types
import Data.Version

import Network.HTTP.Conduit hiding (Proxy)
import Network.Mime

import Servant.Client
import Servant.Multipart

import System.FilePath

import Paths_uow_apis (version)
import Warwick.Common
import Warwick.AEP.FileUpload
import qualified Warwick.AEP.API as AEP

--------------------------------------------------------------------------------

-- | Enumerates AEP instances.
data AEPInstance = Live | Sandbox | CustomInstance BaseUrl
    deriving Show

instance ToJSON AEPInstance where 
    toJSON Live    = String "live"
    toJSON Sandbox = String "sandbox"
    toJSON (CustomInstance url) = 
        object [ "custom" .= url ]

instance FromJSON AEPInstance where 
    parseJSON (String "live")    = pure Live
    parseJSON (String "sandbox") = pure Sandbox
    parseJSON val = flip (withObject "AEPInstance") val $ \obj -> 
        CustomInstance <$> obj .: "custom"

-- | The URL to the production AEP.
liveURL :: BaseUrl
liveURL = BaseUrl Https "altexams.warwick.ac.uk" 443 ""

-- | The URL to the sandbox AEP.
sandboxURL :: BaseUrl
sandboxURL = BaseUrl Https "altexams-sandbox.warwick.ac.uk" 443 ""

instance HasBaseUrl AEPInstance where
    getBaseUrl Live                 = liveURL
    getBaseUrl Sandbox              = sandboxURL
    getBaseUrl (CustomInstance url) = url

-------------------------------------------------------------------------------

type AEP = ReaderT Text (ExceptT APIError ClientM)

-- | `withAEP` @instance sscCookie computation@ performs a @computation@ which
-- uses the Warwick SSO API by connecting to @instance@ using @sscCookie@.
withAEP :: AEPInstance -> Text -> AEP a -> IO (Either APIError a)
withAEP inst sscCookie m = do
    manager <- newManager tlsManagerSettings

    let url  = getBaseUrl inst
    let env  = ClientEnv manager url

    r <- runClientM (runExceptT (runReaderT m sscCookie)) (env Nothing)

    case r of
        Left serr -> pure $ Left $ TransportError serr
        Right res -> pure res

-------------------------------------------------------------------------------

-- | `uploadFile` @assessmentID file overwrite@ uploads @file@ as an answer 
-- file to the assessment identified by @assessmentID@. If @overwrite@ is True
-- then an existing file with the same name will be overwritten
uploadFile :: UUID -> FilePath -> Bool -> AEP ()
uploadFile assessmentID filePath overwrite = do
    -- Set SSC cookie for auth
    sscCookie <- ask
    let cookie = "__Host-SSO-SSC-OnlineExams=" <> sscCookie

    -- Make `FileUpload` value. The mimetype is derived from the extension
    let fileName = pack $ takeFileName filePath
    let form = MkFileUpload filePath
                            fileName
                            (decodeUtf8 $ defaultMimeLookup fileName)
                            overwrite
    
    -- Requested by Adam/IDG
    let userAgent = "uow-apis/" <> pack (showVersion version)

    -- The boundary to use for the request
    boundary <- liftIO genBoundary

    void $ lift $ lift $ AEP.uploadFile cookie
                                        assessmentID
                                        True
                                        userAgent
                                        (boundary, form)

--------------------------------------------------------------------------------
