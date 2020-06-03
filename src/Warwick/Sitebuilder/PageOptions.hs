--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Sitebuilder.PageOptions (
    PageOptions(..), 
    optsToXML, 
    defaultPageOpts
) where 

--------------------------------------------------------------------------------

import Data.Maybe (catMaybes)
import Data.List (intersperse)
import Data.Text (Text, pack)
import Data.XML.Types

import Text.Atom.Feed
import Text.Atom.Feed.Export

--------------------------------------------------------------------------------

data PageOptions = PageOptions {
    poSearchable :: Maybe Bool,
    poVisible :: Maybe Bool,
    poSpanRHS :: Maybe Bool,
    poDeleted :: Maybe Bool,
    poDescription :: Maybe Text,
    poKeywords :: Maybe [Text],
    poLinkCaption :: Maybe Text,
    poPageHeading :: Maybe Text,
    poTitleBarCaption :: Maybe Text,
    poPageOrder :: Maybe Int,
    poCommentable :: Maybe Bool,
    poCommentsVisibleToCommentersOnly :: Maybe Bool,
    poLayout :: Maybe Text,
    poEditComment :: Maybe Text
} deriving Show

-- | 'optsToXML' @opts@ converts @opts@ to an array of XML elements
optsToXML :: PageOptions -> [Element]
optsToXML PageOptions{..} = catMaybes [
        xmlTextContent "sitebuilder:searchable" <$> 
            (TextString . pack . show <$> poSearchable),
        xmlTextContent "sitebuilder:visibility" <$> 
            (TextString . pack . show <$> poVisible),
        xmlTextContent "sitebuilder:span-rhs" <$> 
            (TextString . pack . show <$> poSpanRHS),
        xmlTextContent "sitebuilder:deleted" <$> 
            (TextString . pack . show <$> poDeleted),
        xmlTextContent "sitebuilder:description" <$> 
            (TextString <$> poDescription),
        xmlTextContent "sitebuilder:keywords" <$> 
            (TextString . mconcat . intersperse ", " <$> poKeywords),
        xmlTextContent "sitebuilder:link-caption" <$> 
            (TextString <$> poLinkCaption),
        xmlTextContent "sitebuilder:page-heading" <$> 
            (TextString <$> poPageHeading),
        xmlTextContent "sitebuilder:title-bar-caption" <$> 
            (TextString <$> poTitleBarCaption),
        xmlTextContent "sitebuilder:page-order" <$> 
            (TextString . pack . show <$> poPageOrder),
        xmlTextContent "sitebuilder:commentable" <$> 
            (TextString . pack . show <$> poCommentable),
        xmlTextContent "sitebuilder:comments-visible-to-commenters-only" <$> 
            (TextString . pack . show <$> poCommentsVisibleToCommentersOnly),
        xmlTextContent "sitebuilder:layout" <$> 
            (TextString <$> poLayout),
        xmlTextContent "sitebuilder:edit-comment" <$> 
            (TextString <$> poEditComment)
    ]

-- | 'defaultPageOpts' represents the default value for PageOptions (all fields
--   are Nothing)
defaultPageOpts :: PageOptions
defaultPageOpts = PageOptions{
    poSearchable = Nothing,
    poVisible = Nothing,
    poSpanRHS = Nothing,
    poDeleted = Nothing,
    poDescription = Nothing,
    poKeywords = Nothing,
    poLinkCaption = Nothing,
    poPageHeading = Nothing,
    poTitleBarCaption = Nothing,
    poPageOrder = Nothing,
    poCommentable = Nothing,
    poCommentsVisibleToCommentersOnly = Nothing,
    poLayout = Nothing,
    poEditComment = Nothing
}
