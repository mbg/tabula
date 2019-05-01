module Warwick.DownloadSubmission where

-------------------------------------------------------------------------------

import Control.Monad.IO.Class

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as C8

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Simple hiding (httpLbs, withResponse)
import qualified Network.HTTP.Client.Conduit as C

import           Network.HTTP.Conduit         (http, parseUrl, responseBody,
                                               responseHeaders)
import           Network.HTTP.Types           (hContentLength, statusCode)

import System.IO (withFile, IOMode(..))

import Servant.API.BasicAuth
import Servant.Client hiding (responseBody, responseHeaders)

import Warwick.Tabula.TabulaSession
import Warwick.Tabula.Types
import Warwick.Tabula.Coursework

-------------------------------------------------------------------------------

buildDownloadURL :: BaseUrl
                 -> ModuleCode
                 -> AssignmentID
                 -> String
                 -> FilePath
                 -> String
buildDownloadURL url mc aid subid fn = concat
    [ baseUrlPath url
    , "/module/"
    , C8.unpack (moduleCode mc)
    , "/assignments/"
    , C8.unpack (toASCIIBytes $ unAssignmentID aid)
    , "/submissions/"
    , subid
    , "/"
    , fn
    ]

downloadSubmission :: String
                   -> ModuleCode
                   -> AssignmentID
                   -> String
                   -> FilePath
                   -> FilePath
                   -> Tabula ()
downloadSubmission sid mc aid subid fn out = do
    manager            <- tabulaManager
    baseURL            <- tabulaURL
    BasicAuthData {..} <- tabulaAuthData
    req <- parseRequest ("https://" ++ baseUrlHost baseURL ++ buildDownloadURL baseURL mc aid subid fn)
    let
        request
            = applyBasicAuth basicAuthUsername basicAuthPassword
            $ setRequestMethod "GET"
            -- $ setRequestPath url
            $ setRequestSecure True
            $ setRequestPort (baseUrlPort baseURL)
            $ setRequestHost (BS.packChars $ baseUrlHost baseURL)
            $ setRequestCheckStatus
            $ req
    response <- liftIO $ httpLbs request manager
    liftIO $ LBS.writeFile out (responseBody response)
    return ()

data TabulaDownloadCallbacks a = Callbacks {
    onWrapper  :: IO () -> IO (),
    onLength   :: Int -> IO a,
    onUpdate   :: a -> C8.ByteString -> IO (),
    onComplete :: a -> IO ()
}

downloadSubmissionWithCallbacks :: String
                   -> ModuleCode
                   -> AssignmentID
                   -> String
                   -> FilePath
                   -> FilePath
                   -> TabulaDownloadCallbacks a
                   -> Tabula ()
downloadSubmissionWithCallbacks sid mc aid subid fn out (Callbacks {..}) = do
    manager            <- tabulaManager
    baseURL            <- tabulaURL
    BasicAuthData {..} <- tabulaAuthData
    req <- parseRequest ("https://" ++ baseUrlHost baseURL ++ buildDownloadURL baseURL mc aid subid fn)
    let
        request
            = applyBasicAuth basicAuthUsername basicAuthPassword
            $ setRequestMethod "GET"
            -- $ setRequestPath url
            $ setRequestSecure True
            $ setRequestPort (baseUrlPort baseURL)
            $ setRequestHost (BS.packChars $ baseUrlHost baseURL)
            $ setRequestCheckStatus
            $ req { responseTimeout = responseTimeoutNone }
    --res <- liftIO $ runConduitRes $ http request manager
    liftIO $ onWrapper $ withFile out WriteMode $ \h -> withResponse request manager $ \response -> do
        --putStrLn $ "The status code was: " ++
         --          show (statusCode $ responseStatus response)

        let Just cl = lookup hContentLength (responseHeaders response)
        r <- onLength (read (C8.unpack cl))

        let loop = do
                bs <- brRead $ responseBody response
                if BS.null bs
                    then onComplete r
                    else do
                        onUpdate r bs
                        BS.hPut h bs
                        loop
        loop


    --response <- liftIO $ httpLbs request manager
    --liftIO $ LBS.writeFile out (responseBody response)
    return ()
