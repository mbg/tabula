--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Sitebuilder.FileOptions where 

--------------------------------------------------------------------------------

import Data.Text (Text)

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
