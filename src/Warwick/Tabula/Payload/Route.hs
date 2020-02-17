--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019-2020 Michael B. Gale (m.gale@warwick.ac.uk)                 --
--------------------------------------------------------------------------------

module Warwick.Tabula.Payload.Route where 

--------------------------------------------------------------------------------

import Data.Aeson
import Data.Text

import Warwick.Tabula.Types

--------------------------------------------------------------------------------

-- | Represents information about a route (course).
data Route = Route {
    -- | A value indicating whether the module is currently active.
    routeCode :: Text,
    -- | The degree type.
    routeDegreeType :: Text,
    -- | The name of the route.
    routeName :: Text,
    -- | A value indicating whether the route is active.
    routeActive :: Maybe Bool
} deriving (Eq, Show)

instance FromJSON Route where 
    parseJSON = withObject "Route" $ \obj ->
        Route <$> obj .: "code" 
              <*> obj .: "degreeType"
              <*> obj .: "name"
              <*> obj .:? "active"

--------------------------------------------------------------------------------
