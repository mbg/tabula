--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019-2020 Michael B. Gale (m.gale@warwick.ac.uk)                 --
--------------------------------------------------------------------------------

module Warwick.Tabula.Payload.BaseModule where 

--------------------------------------------------------------------------------

import Data.Aeson
import Data.Text

import Warwick.Tabula.Types

--------------------------------------------------------------------------------

-- | Represents information about a module.
data BaseModule = BaseModule {
    -- | A value indicating whether the module is currently active.
    mActive :: Maybe Bool,
    -- | The module code.
    mCode :: Text,
    -- | The name of the module.
    mName :: Text
} deriving (Eq, Show)

instance FromJSON BaseModule where 
    parseJSON = withObject "BaseModule" $ \obj ->
        BaseModule <$> obj .:? "active" 
                   <*> obj .: "code"
                   <*> obj .: "name"

--------------------------------------------------------------------------------