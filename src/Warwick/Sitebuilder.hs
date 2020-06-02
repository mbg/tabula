--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Warwick.Sitebuilder (
    module Warwick.Config,
    module Warwick.Common,
    module API,

    SitebuilderInstance(..),

    editPage,
    editPageFromFile,
    pageInfo,
    uploadFile,
    purge
) where 

--------------------------------------------------------------------------------

import Control.Monad.Except

import Data.Aeson
import qualified Data.ByteString.Internal as BS
import Data.Text
import Data.Text.Encoding

import Network.HTTP.Client
import Network.HTTP.Simple hiding (httpLbs, withResponse)
import Network.Mime

import Servant.API.BasicAuth
import Servant.Client

import Warwick.Config
import Warwick.Common

import qualified Warwick.Sitebuilder.API as API
import qualified Warwick.Sitebuilder.PageInfo as API
import qualified Warwick.Sitebuilder.PageUpdate as API

--------------------------------------------------------------------------------

-- | Enumerates Sitebuilder instances.
data SitebuilderInstance = Live | CustomInstance BaseUrl

instance ToJSON SitebuilderInstance where 
    toJSON Live    = String "live"
    toJSON (CustomInstance url) = 
        object [ "custom" .= url ]

instance FromJSON SitebuilderInstance where 
    parseJSON (String "live")    = pure Live 
    parseJSON val = flip (withObject "SitebuilderInstance") val $ \obj -> 
        CustomInstance <$> obj .: "custom"

-- | The URL to the Sitebuilder API.
liveURL :: BaseUrl
liveURL = BaseUrl Https "sitebuilder.warwick.ac.uk" 443 "/sitebuilder2"

instance HasBaseUrl SitebuilderInstance where
    getBaseUrl Live                 = liveURL
    getBaseUrl (CustomInstance url) = url

-------------------------------------------------------------------------------

-- | 'editPage' @path update@ updates the page at @path@ with @update@.
editPage :: Text -> API.PageUpdate -> Warwick ()
editPage page content = do 
    authData <- getAuthData
    lift $ lift $ API.editPage authData (Just page) (Just "single") content 

-- | 'editPageFromFile' @page comment filepath@ updates the page at @page@ with 
-- the contents of the file located at @filepath@. @comment@ is used as the
-- change note for this edit.
editPageFromFile :: Text -> Text -> FilePath -> Warwick ()
editPageFromFile page comment fp = do 
    contents <- liftIO $ readFile fp
    editPage page API.PageUpdate{
        puContents = Just $ pack contents,
        puOptions = API.defaultPageOpts { API.poEditComment = Just comment }
    }

-- | 'pageInfo' @path@ retrieves information about the page at @path@.
pageInfo :: Text -> Warwick API.PageInfo
pageInfo page = do 
    authData <- getAuthData
    lift $ lift $ API.pageInfo authData (Just page)

-- | 'purge' @path@ purges the page or file located at @path@.
purge :: Text -> Warwick ()
purge page = do 
    authData <- getAuthData
    lift $ lift $ API.purge authData (Just page) (Just "single")

-------------------------------------------------------------------------------

-- | 'uploadFile' @path slug filepath@ uploads the file located at @filepath@
-- to the page located at @path@. @slug@ is used to determine the name of the
-- file that should be created at @path@. 
uploadFile :: Text -> Text -> FilePath -> Warwick ()
uploadFile page slug fp = do 
    manager            <- getManager
    baseURL            <- getURL
    BasicAuthData {..} <- getAuthData
    req <- parseRequest ("https://" ++ baseUrlHost baseURL ++ baseUrlPath baseURL ++ "/edit/atom/file.htm?page=" ++ unpack page)
    let
        mimeType = defaultMimeLookup (pack fp) 
        request
            = applyBasicAuth basicAuthUsername basicAuthPassword
            $ setRequestMethod "POST"
            $ setRequestSecure True
            $ setRequestPort (baseUrlPort baseURL)
            $ setRequestHost (BS.packChars $ baseUrlHost baseURL)
            $ setRequestCheckStatus
            $ setRequestHeader "Slug" [encodeUtf8 slug]
            $ setRequestHeader "Content-Type" [mimeType]
            $ setRequestBodyFile fp
            $ req
    response <- liftIO $ httpLbs request manager
    pure ()

--------------------------------------------------------------------------------