--------------------------------------------------------------------------------
-- Haskell bindings for the Tabula API                                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Tabula.Payload.CourseDetails ( CourseDetails(..) ) where

--------------------------------------------------------------------------------

import Data.Aeson
import Data.Text

--------------------------------------------------------------------------------

-- | Represents basic information about a degree programme.
data CourseDetails = CourseDetails {
    -- | The course code (e.g. "UCSA-G408").
    courseCode :: Text,
    -- | The course name (e.g. "Computer Systems Engineering").
    courseName :: Text,
    -- | The course type (e.g. "UG").
    courseType :: Text
} deriving (Eq, Show)

instance FromJSON CourseDetails where
    parseJSON = withObject "CourseDetails" $ \v ->
        CourseDetails <$> v .: "code"
                      <*> v .: "name"
                      <*> v .: "type"

--------------------------------------------------------------------------------
