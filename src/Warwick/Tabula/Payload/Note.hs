--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Tabula.Payload.Note (
    AttendanceNote(..)
) where 

--------------------------------------------------------------------------------

import Data.Aeson
import Data.Text

import Warwick.Common
import Warwick.Tabula.Attachment

--------------------------------------------------------------------------------

data AttendanceNote = AttendanceNote {
    anAbsenceType :: Text, 
    anContents :: Maybe Text,
    anUpdatedDate :: Date,
    anUpdatedBy :: Text,
    anAttachment :: Maybe Attachment
} deriving (Eq, Show)

instance FromJSON AttendanceNote where 
    parseJSON = withObject "AttendanceNote" $ \obj ->
        AttendanceNote <$> obj .: "absenceType"
                       <*> obj .:? "contents"
                       <*> obj .: "updatedDate"
                       <*> obj .: "updatedBy"
                       <*> obj .:? "attachment"

--------------------------------------------------------------------------------