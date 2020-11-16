-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Warwick.Sitebuilder (
    module Warwick.Config,
    module Warwick.Common,
    module API,

    SitebuilderInstance(..),

    createPage,
    createPageWithRHSFromFile,
    createPageFromFile,
    editPage,
    editPageFromFile,
    editPageRHSFromFile,
    pageInfo,
    fileInfo,
    uploadFile,
    deletePage,
    restorePage,
    editFileProps,
    deleteFile,
    restoreFile,
    purgePage,
    purgeFile
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
import qualified Warwick.Sitebuilder.PageOptions as API
import qualified Warwick.Sitebuilder.Page as API
import qualified Warwick.Sitebuilder.FileOptions as API
import qualified Warwick.Sitebuilder.FileInfo as API

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

-- | `createPage` @path pageData@ creates a page at location @path@ with data
--   @pageData@
createPage :: Text -> API.Page -> Warwick ()
createPage path pageData = do
    authData <- getAuthData
    lift $ lift $ API.createPage authData (Just path) pageData

-- | `createPageWithRHSFromFile` @path title pageName filepath rhsFilepath@ 
-- creates a page @pageName@ at the location @path@ with title @title@ and  
-- the contents of @filePath@ as the page contents and the contents of
-- @rhsFilepath@ as the RHS contents of the page.
createPageWithRHSFromFile :: Text -> Text -> Text -> FilePath -> FilePath 
                          -> Warwick ()
createPageWithRHSFromFile path title name fp rhsfp = do
    contents <- liftIO $ readFile fp
    rhsContents <- liftIO $ readFile rhsfp
    createPage path $ API.Page{
        pcTitle = title,
        pcContents = pack contents,
        pcRhsContents = Just $ pack rhsContents,
        pcPageName = name,
        pcOptions = API.defaultPageOpts
    }

-- | `createPageFromFile` @path title pageName filepath@ creates a page @pageName@
--   at the location @path@ with title @title@ and the contents of @filePath@ as 
--   the page contents
createPageFromFile :: Text -> Text -> Text -> FilePath -> Warwick ()
createPageFromFile path title name fp = do
    contents <- liftIO $ readFile fp
    createPage path $ API.Page{
        pcTitle = title,
        pcContents = pack contents,
        pcRhsContents = Nothing,
        pcPageName = name,
        pcOptions = API.defaultPageOpts
    }

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
        puRhsContents = Nothing,
        puOptions = API.defaultPageOpts { API.poEditComment = Just comment }
    }

-- | 'editPageRHSFromFile' @page comment filepath@ updates the RHS of @page@  
-- with the contents of the file located at @filepath@. @comment@ is used as 
-- the change note for this edit.
editPageRHSFromFile :: Text -> Text -> FilePath -> Warwick ()
editPageRHSFromFile page comment fp = do 
    contents <- liftIO $ readFile fp
    editPage page API.PageUpdate{
        puContents = Nothing,
        puRhsContents = Just $ pack contents,
        puOptions = API.defaultPageOpts { API.poEditComment = Just comment }
    }

-- | 'pageInfo' @path@ retrieves information about the page at @path@.
pageInfo :: Text -> Warwick API.PageInfo
pageInfo page = do 
    authData <- getAuthData
    lift $ lift $ API.pageInfo authData (Just page)

-- | 'fileInfo' @path@ retrieves information about the file at @path@.
fileInfo :: Text -> Warwick API.FileInfo
fileInfo page = do 
    authData <- getAuthData
    lift $ lift $ API.fileInfo authData (Just page)

-- | 'changeDeleteStatus' @deleted path@ sets the deleted status for the page at 
--   @path@ to @deleted@
changeDeleteStatus :: Bool -> Text -> Warwick ()
changeDeleteStatus deleted page = do
    authData <- getAuthData
    lift $ lift $ API.editPage authData (Just page) (Just "single") $ 
        API.PageUpdate{
            puContents = Nothing,
            puRhsContents = Nothing,
            puOptions = API.defaultPageOpts{ API.poDeleted = Just deleted }
        }

-- | 'deletePage' @path@ sets the deleted status to true for the page at @path@
deletePage :: Text -> Warwick ()
deletePage = changeDeleteStatus True

-- | 'restorePage' @path@ sets the deleted status to false for the page at @path@
restorePage :: Text -> Warwick ()
restorePage = changeDeleteStatus False

-- | 'editFileProps' @path options@ updates the properties for the file at @path@
--   with the changes in @options@
editFileProps :: Text -> API.FileOptions -> Warwick ()
editFileProps file opts = do
    authData <- getAuthData
    lift $ lift $ API.editFileProps authData (Just file) (Just "single") opts

-- | 'deletePage' @path@ sets the deleted status to true for the file at @path@
deleteFile :: Text -> Warwick ()
deleteFile = flip editFileProps API.defaultFileOpts { API.foDeleted = Just True }

-- | 'restorePage' @path@ sets the deleted status to false for the file at @path@
restoreFile :: Text -> Warwick ()
restoreFile = flip editFileProps API.defaultFileOpts { API.foDeleted = Just False }

-- | 'purgePage' @path@ purges the page located at @path@.
purgePage :: Text -> Warwick ()
purgePage page = do 
    authData <- getAuthData
    lift $ lift $ API.purgePage authData (Just page) (Just "single")

-- | 'purgeFile' @path@ purges the file located at @path@.
purgeFile :: Text -> Warwick ()
purgeFile page = do 
    authData <- getAuthData
    lift $ lift $ API.purgeFile authData (Just page) (Just "single")

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