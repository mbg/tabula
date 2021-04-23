-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.Exams.ExamProfile ( ExamProfile(..) ) where 

-------------------------------------------------------------------------------

import Data.Aeson
import Data.Text

import Warwick.Common
import Warwick.Response

-------------------------------------------------------------------------------

-- | Represents an exam profile which typically corresponds to a particular
-- exam period.
data ExamProfile = MkExamProfile {
    -- | The unique identifier of the exam profile.
    examProfileCode :: Text,
    -- | A descriptive name of the exam profile.
    examProfileName :: Text,
    -- | The academic year to which this profile relates (e.g. "20/21").
    examProfileAcademicYear :: Text,
    -- | The start date for the exam period represented by this profile.
    examProfileStartDate :: Date,
    -- | The end date for the exam period represented by this profile.
    examProfileEndDate :: Date,
    -- | A value indicating whether the profile has been published.
    examProfilePublished :: Bool,
    -- | A value indicating whether seat numbers have been published.
    examProfileSeatNumbersPublished :: Bool
} deriving (Eq, Show)

instance HasPayload [ExamProfile] where
    payloadFieldName = "examProfiles"

instance FromJSON ExamProfile where
    parseJSON = withObject "ExamProfile" $ \v ->
        MkExamProfile <$> v .: "code"
                      <*> v .: "name"
                      <*> v .: "academicYear"
                      <*> v .: "startDate"
                      <*> v .: "endDate"
                      <*> v .: "published"
                      <*> v .: "seatNumbersPublished"

-------------------------------------------------------------------------------
