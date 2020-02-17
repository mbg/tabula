--------------------------------------------------------------------------------
-- Haskell bindings for the Tabula API                                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Tabula.Payload.Module where 

--------------------------------------------------------------------------------

import Data.Aeson
import Data.Text

import Warwick.Tabula.Payload.Department
import Warwick.Tabula.Payload.BaseModule
import Warwick.Tabula.Types

--------------------------------------------------------------------------------

-- | Represents information about a module.
data Module = Module {
    -- | The base module details.
    mBaseModule :: BaseModule,
    -- | Information about the department responsible for the module.
    mAdminDepartment :: DepartmentR
} deriving (Eq, Show)

instance FromJSON Module where 
    parseJSON = withObject "Module" $ \obj ->
        Module <$> parseJSON (Object obj) 
               <*> obj .: "adminDepartment"

instance HasPayload Module where 
    payloadFieldName _ = "module"

--------------------------------------------------------------------------------