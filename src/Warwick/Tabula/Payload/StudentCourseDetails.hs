--------------------------------------------------------------------------------
-- Haskell bindings for the Tabula API                                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Tabula.Payload.StudentCourseDetails where

--------------------------------------------------------------------------------

import Data.Aeson
import Data.Text

import Warwick.Tabula.Types
import Warwick.Tabula.Payload.CourseDetails

--------------------------------------------------------------------------------

-- | Represents information about a student's status on a course.
data StudentCourseDetails = StudentCourseDetails {
    -- | The "student course join" code for a student in SITS.
    scdSCJCode :: Text,
    -- | The "student programme route" code for a student in SITS.
    scdSPRCode :: Text,
    -- | The code for the current level for this student course details.
    scdLevelCode :: Text,
    -- | The date that this student course details begins.
    scdBeginDate :: Date,
    -- | The date that this student course details ended.
    scdEndDate :: Maybe Date,
    -- | The date that this student course details is expected to end.
    scdExpectedEndDate :: Date,
    -- | The expected length of the course, in years.
    scdCourseYearLength :: Text,
    -- | Whether this is currently the most significant student course details -
    -- this will be `true` for only one student course details per student.
    scdMostSignificant :: Bool,
    -- | The SITS code for the reason for transfer.
    scdReasonForTransferCode :: Maybe Text,
    -- | An object representing the course for this student course details with
    -- properties `code`, `name` and `type`.
    scdCourse :: CourseDetails
    -- TODO: more fields here, see
    -- https://warwick.ac.uk/services/its/servicessupport/web/tabula/api/member/student-course-details-object
}

instance FromJSON StudentCourseDetails where
    parseJSON = withObject "StudentCourseDetails" $ \v ->
        StudentCourseDetails <$> v .: "scjCode"
                             <*> v .: "sprCode"
                             <*> v .: "levelCode"
                             <*> v .: "beginDate"
                             <*> v .:? "endDate"
                             <*> v .: "expectedEndDate"
                             <*> v .: "courseYearLength"
                             <*> v .: "mostSignificant"
                             <*> v .:? "reasonForTransferCode"
                             <*> v .: "course"

-- | `mostSignificantCourse` @courses@ finds the most significant course in
-- @courses@ if there is one (which there should always be).
mostSignificantCourse :: [StudentCourseDetails] -> Maybe StudentCourseDetails
mostSignificantCourse xs = case [c | c <- xs, scdMostSignificant c] of
    [r] -> Just r
    _   -> Nothing

--------------------------------------------------------------------------------
