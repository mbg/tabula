--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Sitebuilder.Page (Page(..)) where 

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

data Page = Page {
    pcTitle :: Text,
    pcContents :: Text,
    pcPageName :: Text,
    pcOptions :: PageOptions
} deriving Show

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
            entryOther = xmlTextContent "sitebuilder:page-name" (TextString pcPageName) : optsToXML pcOptions
        } 
