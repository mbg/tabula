
module Warwick.Tabula.Coursework where

import GHC.Generics

import Data.Aeson
import qualified Data.HashMap.Lazy as HM

import Warwick.Tabula.Types
import Warwick.Tabula.Attachment

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
    --assignmentStudentMembership :: (),
    --assignmentSitsLinks :: [()],
    assignmentOpenEnded :: Bool,
    assignmentOpened :: Bool,
    assignmentClosed :: Bool,
    assignmentOpenDate :: TabulaDateTime,
    assignmentCloseDate :: TabulaDateTime,
    assignmentFeedbackDeadline :: Maybe TabulaDate,
    assignmentFeedback :: Int,
    assignmentUnpublishedFeedback :: Int
} deriving (Show, Generic)

instance HasPayload [Assignment] where
    payloadFieldName _ = "assignments"

instance FromJSON Assignment where
    parseJSON = parseTabulaJSON

-- | Represents coursework submissions.
data Submission = Submission {
    -- | The unique ID of the submission.
    submissionID                 :: String,
    -- | Indicates whether the submission has been downloaded.
    submissionDownloaded         :: Bool,
    -- | Indicates when the submission was made.
    submissionSubmittedDate      :: TabulaDateTime,
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
