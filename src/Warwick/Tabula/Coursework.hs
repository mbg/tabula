
module Warwick.Tabula.Coursework where

import GHC.Generics

import Data.Aeson
import qualified Data.HashMap.Lazy as HM

import Warwick.Tabula.Types
import Warwick.Tabula.Attachment

-- | Represents an entry in a list of users who are registered for an
-- assignment on Tabula.
data StudentMember = StudentMember {
    smUserName :: String,
    smUserID   :: String
} deriving Show

instance FromJSON StudentMember where
    parseJSON = withObject "StudentMembership" $ \v ->
        StudentMember <$> v .: "userId"
                      <*> v .: "universityId"

-- | Represents a list of users who are registered for an assignment.
data StudentMembership = StudentMembership {
    -- | The total number of students linked to the assignment.
    smTotal :: Int,
    -- | The number of students linked to SITS assessment groups.
    smLinkedSITS :: Int,
    -- | The number of extra students added to the assignment and not linked to
    -- SITS.
    smIncluded :: Int,
    -- | The number of students who have been manually removed from the
    -- membership of the assignment.
    smExcluded :: Int,
    -- | The list of users who are registered for the assignment.
    smUsers    :: [StudentMember]
} deriving Show

instance FromJSON StudentMembership where
    parseJSON = withObject "StudentMembership" $ \v ->
        StudentMembership <$> v .: "total"
                          <*> v .: "linkedSits"
                          <*> v .: "included"
                          <*> v .: "excluded"
                          <*> v .: "users"

data Assignment = Assignment {
    assignmentID :: AssignmentID,
    assignmentArchived :: Bool,
    assignmentAcademicYear :: String,
    assignmentName :: String,
    assignmentStudentUrl :: String,
    assignmentCollectMarks :: Bool,
    --assignmentMarkingWorkflow :: Maybe (),
    --assignmentFeedbackTemplate :: Maybe (),
    assignmentSummative :: Bool,
    assignmentDissertation :: Bool,
    assignmentCollectSubmissions :: Bool,
    assignmentDisplayPlagiarismNotice :: Maybe Bool,
    assignmentRestrictSubmissions :: Maybe Bool,
    assignmentAllowLateSubmissions :: Maybe Bool,
    assignmentAllowResubmission :: Maybe Bool,
    assignmentAllowExtensions :: Bool,
    assignmentFileAttachmentLimit :: Maybe Int,
    assignmentFileAttachmentTypes :: Maybe [String],
    assignmentSubmissionFormText :: Maybe String,
    assignmentWordCountMin :: Maybe Int,
    assignmentWordCountMax :: Maybe Int,
    assignmentWordCountConventions :: Maybe String,
    assignmentSubmissions :: Maybe Int,
    assignmentUnapprovedExtensions :: Int,
    assignmentStudentMembership :: StudentMembership,
    --assignmentSitsLinks :: [()],
    assignmentOpenEnded :: Bool,
    assignmentOpened :: Bool,
    assignmentClosed :: Bool,
    assignmentOpenDate :: DateTime,
    assignmentCloseDate :: Maybe DateTime,
    assignmentFeedbackDeadline :: Maybe Date,
    assignmentFeedback :: Int,
    assignmentUnpublishedFeedback :: Int
} deriving (Show, Generic)

instance HasPayload [Assignment] where
    payloadFieldName _ = "assignments"

instance HasPayload Assignment where
    payloadFieldName _ = "assignment"

instance FromJSON Assignment where
    parseJSON = parseTabulaJSON

-- | Represents coursework submissions.
data Submission = Submission {
    -- | The unique ID of the submission.
    submissionID                 :: String,
    -- | Indicates whether the submission has been downloaded.
    submissionDownloaded         :: Bool,
    -- | Indicates when the submission was made.
    submissionSubmittedDate      :: DateTime,
    -- | Indicates whether the submission was late.
    submissionLate               :: Bool,
    -- | Indicates whether lateness was authorised.
    submissionAuthorisedLate     :: Bool,
    -- | Indicates the word count, if available.
    submissionWordCount          :: Maybe Int,
    -- | Indicates whether plagiarisim is suspected.
    submissionSuspectPlagiarised :: Bool,
    -- | A list of attachment objects associated with this submission.
    submissionAttachments        :: [Attachment]
} deriving (Show, Generic)

instance FromJSON Submission where
    parseJSON = parseTabulaJSON

instance HasPayload (HM.HashMap String (Maybe Submission)) where
    payloadFieldName _ = "submissions"
