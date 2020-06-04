--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019-2020 Michael B. Gale (m.gale@warwick.ac.uk)                 --
--------------------------------------------------------------------------------

module Warwick.Sitebuilder.PageEdit (PageEdit(..)) where 

--------------------------------------------------------------------------------

import Data.Aeson
import Data.Text

--------------------------------------------------------------------------------

data PageEdit = PageEdit {
    peUserName :: Text,
    peDate :: Int
} deriving Show

instance FromJSON PageEdit where 
    parseJSON = withObject "PageEdit" $ \obj ->
        PageEdit <$> obj .: "user" <*> obj .: "date"
        