--------------------------------------------------------------------------------
-- Haskell bindings for the Tabula API                                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Tabula.Payload.Department where 

--------------------------------------------------------------------------------

import Data.Aeson
import Data.Text 

import Warwick.Tabula.Types
import Warwick.Tabula.Payload.BaseModule
import Warwick.Tabula.Payload.Route

--------------------------------------------------------------------------------

-- | Represents (reduced) information about a department.
data DepartmentR = DepartmentR {
    -- | The unique department code.
    dCode :: Text, 
    -- | The department's name.
    dName :: Text
} deriving (Eq, Show)

instance FromJSON DepartmentR where 
    parseJSON = withObject "Department (reduced)" $ \obj ->
        DepartmentR <$> obj .: "code" 
                    <*> obj .: "name"

data Department = Department {
    dBasics :: DepartmentR,
    dFullName :: Text,
    dShortName :: Text,
    dModules :: [BaseModule],
    dRoutes :: [Route]
} deriving (Eq, Show)

instance FromJSON Department where 
    parseJSON = withObject "Department" $ \obj ->
        Department <$> (parseJSON $ Object obj) 
                   <*> obj .: "fullName"
                   <*> obj .: "shortName"
                   <*> obj .: "modules"
                   <*> obj .: "routes"

instance HasPayload Department where 
    payloadFieldName _ = "department"

--------------------------------------------------------------------------------
