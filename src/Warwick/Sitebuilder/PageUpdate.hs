-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.Sitebuilder.PageUpdate where 

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

data PageUpdate = PageUpdate {
    -- | The HTML to update the main page content with or `Nothing` to keep
    -- the existing content.
    puContents :: Maybe Text,
    -- | The HTML to update the RHS page content with or `Nothing` to keep the
    -- existing content.
    puRhsContents :: Maybe Text,
    -- | Other page options to change.
    puOptions :: PageOptions
} deriving (Eq, Show)

instance MimeRender ATOM PageUpdate where 
    mimeRender _ PageUpdate{..} = 
        renderLBS def $ 
        elementToDoc $ 
        xmlEntry $ 
        (nullEntry "" (TextString "") "") {
            entryContent = HTMLContent <$> puContents,
            entryAttrs = [
                ("xmlns:sitebuilder", [
                    ContentText "http://go.warwick.ac.uk/elab-schemas/atom"
                ])
            ],
            entryOther = 
                let other = optsToXML puOptions
                    rhsElement = puRhsContents >>= \rhsContent -> pure $
                        (xmlContent $ HTMLContent rhsContent){
                            elementName = "sitebuilder:rhs-content"
                        } 
                in maybe other (: other) rhsElement 
        } 

--------------------------------------------------------------------------------
