--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Sitebuilder.FileOptions (
    FileOptions(..),
    defaultFileOpts,
    fileOptsToXML
) where 

--------------------------------------------------------------------------------

import Data.Maybe (catMaybes)
import Data.List (intersperse)
import Data.Text (Text, pack)
import Data.XML.Types 

import Text.Atom.Feed
import Text.Atom.Feed.Export

--------------------------------------------------------------------------------

data FileOptions = FileOptions {
    foTitle :: Maybe Text,
    foSearchable :: Maybe Bool,
    foVisible :: Maybe Bool,
    foDeleted :: Maybe Bool,
    foDescription :: Maybe Text,
    foKeywords :: Maybe [Text],
    foPageOrder :: Maybe Int
}

defaultFileOpts :: FileOptions
defaultFileOpts = FileOptions {
    foTitle = Nothing,
    foSearchable = Nothing,
    foVisible = Nothing,
    foDeleted = Nothing,
    foDescription = Nothing,
    foKeywords = Nothing,
    foPageOrder = Nothing
}

fileOptsToXML :: FileOptions -> [Element]
fileOptsToXML FileOptions{..} = catMaybes [
        xmlTextContent "title" <$>
            (TextString . pack . show <$> foTitle),
        xmlTextContent "sitebuilder:searchable" <$> 
            (TextString . pack . show <$> foSearchable),
        xmlTextContent "sitebuilder:visibility" <$> 
            (TextString . pack . show <$> foVisible),
        xmlTextContent "sitebuilder:deleted" <$> 
            (TextString . pack . show <$> foDeleted),
        xmlTextContent "sitebuilder:description" <$> 
            (TextString <$> foDescription),
        xmlTextContent "sitebuilder:keywords" <$> 
            (TextString .  mconcat . intersperse ", " <$> foKeywords),
        xmlTextContent "sitebuilder:page-order" <$> 
            (TextString . pack . show <$> foPageOrder)
    ]
