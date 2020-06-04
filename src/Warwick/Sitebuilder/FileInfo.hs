--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019-2020 Michael B. Gale (m.gale@warwick.ac.uk)                 --
--------------------------------------------------------------------------------

module Warwick.Sitebuilder.FileInfo where 

--------------------------------------------------------------------------------

import Data.Aeson
import Data.Text

import Warwick.Sitebuilder.PageEdit

--------------------------------------------------------------------------------

data FileProperties = FileProperties {
    fpIncludeLegacyJS :: Bool,
    fpShowInLocalNavigation :: Bool,
    fpPageOrder :: Int, 
    fpAllowSearchEngines :: Bool,
    fpEscapeHtml :: Bool,
    fpSupportsPagesToGo :: Bool,
    fpDeferJS :: Bool
} deriving Show

instance FromJSON FileProperties where 
    parseJSON = withObject "FileProperties" $ \obj ->
        FileProperties <$> obj .: "includeLegacyJavascript"
                       <*> obj .: "showInLocalNavigation"
                       <*> obj .: "pageOrder"
                       <*> obj .: "allowSearchEngines"
                       <*> obj .: "escapeHtml"
                       <*> obj .: "supportsPagesToGo"
                       <*> obj .: "deferJs"

data FileInfo = FileInfo {
    fileID :: Text,
    fileParent :: Text,
    fileName :: Text,
    fileLinkCaption :: Text, 
    fileKeywords :: [Text],
    fileCanEdit :: Bool,
    fileCanAdmin :: Bool,
    fileMimeType :: Text,
    fileEdited :: PageEdit, 
    filePath :: Text,
    filePageType :: Text,
    fileResizeableImage :: Bool, 
    fileChildren :: [Text],
    filePubliclyVisible :: Bool,
    fileContact :: Text,
    fileImage :: Bool,
    fileCreator :: Text,
    fileIsSiteRoot :: Bool,
    fileCreated :: PageEdit,
    fileCanContribute :: Bool,
    filePropertiesUpdated :: PageEdit,
    fileURL :: Text,
    fileContentUpdated :: PageEdit,
    fileDeleted :: Bool,
    fileSiteRoot :: Text,
    fileSize :: Int,
    fileUpdated :: PageEdit,
    fileProperties :: FileProperties
} deriving Show

instance FromJSON FileInfo where 
    parseJSON = withObject "FileInfo" $ \obj ->
        FileInfo <$> obj .: "id"
                 <*> obj .: "parent"
                 <*> obj .: "fileName"
                 <*> obj .: "linkCaption"
                 <*> obj .: "keywords"
                 <*> obj .: "canEdit"
                 <*> obj .: "canAdmin"
                 <*> obj .: "mimeType"
                 <*> obj .: "editedUpdated"
                 <*> obj .: "path"
                 <*> obj .: "pageType"
                 <*> obj .: "resizeableImage"
                 <*> obj .: "children"
                 <*> obj .: "publiclyVisible"
                 <*> obj .: "pageContact"
                 <*> obj .: "image"
                 <*> obj .: "creator"
                 <*> obj .: "isSiteRoot"
                 <*> obj .: "created"
                 <*> obj .: "canContribute"
                 <*> obj .: "propertiesUpdated"
                 <*> obj .: "url"
                 <*> obj .: "contentUpdated"
                 <*> obj .: "deleted"
                 <*> obj .: "siteRoot"
                 <*> obj .: "fileSize"
                 <*> obj .: "updated"
                 <*> obj .: "properties"

--------------------------------------------------------------------------------
