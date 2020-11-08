-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.Sitebuilder.Page (Page(..)) where 

--------------------------------------------------------------------------------

import Data.Default
import Data.Text (Text)
import Data.XML.Types 

import Text.Atom.Feed
import Text.Atom.Feed.Export
import Text.XML (renderLBS)

import Servant.API

import Warwick.Sitebuilder.Atom
import Warwick.Sitebuilder.PageOptions

--------------------------------------------------------------------------------

-- | Represents information required to create a new Sitebuilder page.
data Page = Page {
    -- | The title of the page to create.
    pcTitle :: Text,
    -- | The contents of the page to create.
    pcContents :: Text,
    -- | The RHS contents of the page to create.
    pcRhsContents :: Maybe Text,
    -- | The name of the page to create.
    pcPageName :: Text,
    -- | Other options for the new page.
    pcOptions :: PageOptions
} deriving (Eq, Show)

instance MimeRender ATOM Page where 
    mimeRender _ Page{..} = 
        renderLBS def $ 
        elementToDoc $ 
        xmlEntry $ 
        (nullEntry "" (TextString "") "") {
            entryTitle = TextString pcTitle,
            entryContent = Just $ HTMLContent pcContents,
            entryAttrs = [
                ("xmlns:sitebuilder", [
                    ContentText "http://go.warwick.ac.uk/elab-schemas/atom"
                ])
            ],
            entryOther = 
                let other = xmlTextContent "sitebuilder:page-name" (TextString pcPageName) 
                          : optsToXML pcOptions
                    rhsElement = pcRhsContents >>= \rhsContent -> pure $
                        (xmlContent $ HTMLContent rhsContent){
                            elementName = "sitebuilder:rhs-content"
                        } 
                in maybe other (: other) rhsElement 
        } 

--------------------------------------------------------------------------------
