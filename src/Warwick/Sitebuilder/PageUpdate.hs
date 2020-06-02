--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Sitebuilder.PageUpdate where 

--------------------------------------------------------------------------------

import Data.Text (Text)
import Data.XML.Types 

import Text.Atom.Feed
import Text.Atom.Feed.Export
import Text.XML

import Servant.API

import Warwick.Sitebuilder.Atom
import Warwick.Sitebuilder.PageOptions

--------------------------------------------------------------------------------

data PageUpdate = PageUpdate {
    puContents :: Maybe Text,
    puOptions :: PageOptions
} deriving Show

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
            entryOther = optsToXML puOptions
        } 

--------------------------------------------------------------------------------
