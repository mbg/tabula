--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019-2020 Michael B. Gale (m.gale@warwick.ac.uk)                 --
--------------------------------------------------------------------------------

module Warwick.Sitebuilder.PageInfo where 

--------------------------------------------------------------------------------

import Data.Aeson
import Data.Text

import Warwick.Sitebuilder.PageEdit

--------------------------------------------------------------------------------

data PageProperties = PageProperties {
    ppIncludeLegacyJS :: Bool,
    ppShowInLocalNavigation :: Bool,
    ppPageOrder :: Int, 
    ppHasThumbnail :: Bool,
    ppAllowSearchEngines :: Bool,
    ppEscapeHtml :: Bool,
    ppSupportsPagesToGo :: Bool,
    ppSpanRhs :: Bool,
    ppDeferJS :: Bool
} deriving Show

instance FromJSON PageProperties where 
    parseJSON = withObject "PageProperties" $ \obj ->
        PageProperties <$> obj .: "includeLegacyJavascript"
                       <*> obj .: "showInLocalNavigation"
                       <*> obj .: "pageOrder"
                       <*> obj .: "hasThumbnail"
                       <*> obj .: "allowSearchEngines"
                       <*> obj .: "escapeHtml"
                       <*> obj .: "supportsPagesToGo"
                       <*> obj .: "spanRhs"
                       <*> obj .: "deferJs"

data PageInfo = PageInfo {
    pageID :: Text,
    pageParent :: Text,
    pageLinkCaption :: Text, 
    pageKeywords :: [Text],
    pageCanEdit :: Bool,
    pageCanAdmin :: Bool,
    pageDescription :: Text,
    pageShortTitle :: Text, 
    pageMimeType :: Text,
    pageEdited :: PageEdit, 
    pagePath :: Text, 
    pageHeading :: Text,
    pageType :: Text, 
    pageChildren :: [Text],
    pageContentType :: Text,
    pagePubliclyVisible :: Bool,
    pageContact :: Text,
    pageCreator :: Text,
    pageIsSiteRoot :: Bool,
    pageCreated :: PageEdit,
    pageCanContribute :: Bool,
    pagePropertiesUpdated :: PageEdit,
    pageURL :: Text,
    pageContentUpdated :: PageEdit,
    pageDeleted :: Bool,
    pageSiteRoot :: Text,
    pageUpdated :: PageEdit,
    pageProperties :: PageProperties
} deriving Show

instance FromJSON PageInfo where 
    parseJSON = withObject "PageInfo" $ \obj ->
        PageInfo <$> obj .: "id"
                 <*> obj .: "parent"
                 <*> obj .: "linkCaption"
                 <*> obj .: "keywords"
                 <*> obj .: "canEdit"
                 <*> obj .: "canAdmin"
                 <*> obj .: "description"
                 <*> obj .: "shortTitle"
                 <*> obj .: "mimeType"
                 <*> obj .: "editedUpdated"
                 <*> obj .: "path"
                 <*> obj .: "pageHeading"
                 <*> obj .: "pageType"
                 <*> obj .: "children"
                 <*> obj .: "contentType"
                 <*> obj .: "publiclyVisible"
                 <*> obj .: "pageContact"
                 <*> obj .: "creator"
                 <*> obj .: "isSiteRoot"
                 <*> obj .: "created"
                 <*> obj .: "canContribute"
                 <*> obj .: "propertiesUpdated"
                 <*> obj .: "url"
                 <*> obj .: "contentUpdated"
                 <*> obj .: "deleted"
                 <*> obj .: "siteRoot"
                 <*> obj .: "updated"
                 <*> obj .: "properties"

--------------------------------------------------------------------------------
