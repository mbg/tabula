-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.Tabula.StudentAssignment where

--------------------------------------------------------------------------------

import Data.Aeson

import Warwick.Tabula.Types
import Warwick.Tabula.Payload.Extension

--------------------------------------------------------------------------------

data StudentAssignmentFeedback = StudentAssignmentFeedback {
    studentAssignmentFeedbackID :: UUID,
    studentAssignmentFeedbackMark :: Maybe Int,
    studentAssignmentFeedbackGrade :: Maybe String,
    --studentAssignmentFeedbackAdjustments :: (),
    studentAssignmentFeedbackGenericFeedback :: Maybe String,
    studentAssignmentFeedbackComments :: Maybe String,
    --studentAssignmentFeedbackAttachments :: []
    studentAssignmentFeedbackDownloadZIP :: Maybe String,
    studentAssignmentFeedbackDownloadPDF :: Maybe String
} deriving Show

instance FromJSON StudentAssignmentFeedback where
    parseJSON = withObject "StudentAssignmentFeedback" $ \v ->
        StudentAssignmentFeedback <$> v .: "id"
                                  <*> v .:? "mark"
                                  <*> v .:? "grade"
                                  <*> v .:? "genericFeedback"
                                  <*> v .:? "comments"
                                  <*> v .:? "downloadZip"
                                  <*> v .:? "downloadPdf"

data StudentAssignmentSubmission = StudentAssignmentSubmission {
    studentAssignmentSubmissionID             :: UUIDorString,
    studentAssignmentSubmissionLate           :: Bool,
    studentAssignmentSubmissionAuthorisedLate :: Bool,
    --studentAssignmentSubmissionAttachments :: []
    studentAssignmentSubmissionSubmittedDate  :: Maybe DateTime,
    studentAssignmentSubmissionCloseDate      :: Maybe DateTime,
    studentAssignmentSubmissionWordCount      :: Maybe Int
} deriving Show

instance FromJSON StudentAssignmentSubmission where
    parseJSON = withObject "StudentAssignmentSubmission" $ \v ->
        StudentAssignmentSubmission <$> v .: "id"
                                    <*> v .: "late"
                                    <*> v .: "authorisedLate"
                                    <*> v .:? "submittedDate"
                                    <*> v .:? "closeDate"
                                    <*> v .:? "wordCount"

data StudentAssignment = StudentAssignment {
    studentAssignmentID           :: UUID,
    studentAssignmentAcademicYear :: String,
    studentAssignmentName         :: String,
    studentAssignmentStudentURL   :: String,
    studentAssignmentHasSubmission :: Bool,
    studentAssignmentHasFeedback :: Bool,
    studentAssignmentHasExtension :: Bool,
    studentAssignmentHasActiveExtension :: Bool,
    studentAssignmentExtended :: Bool,
    --studentAssignmentModule :: (),
    studentAssignmentLate :: Bool,
    studentAssignmentOpenEnded :: Bool,
    studentAssignmentOpened :: Bool,
    studentAssignmentClosed :: Bool,
    studentAssignmentOpenDate :: DateTime,
    studentAssignmentCloseDate :: Maybe DateTime,
    studentAssignmentSubmission :: Maybe StudentAssignmentSubmission,
    studentAssignmentFeedback  :: Maybe StudentAssignmentFeedback,
    studentAssignmentExtension :: Maybe StudentAssignmentExtension
} deriving Show

instance FromJSON StudentAssignment where
    parseJSON = withObject "StudentAssignment" $ \v ->
        StudentAssignment <$> v .: "id"
                          <*> v .: "academicYear"
                          <*> v .: "name"
                          <*> v .: "studentUrl"
                          <*> v .: "hasSubmission"
                          <*> v .: "hasFeedback"
                          <*> v .: "hasExtension"
                          <*> v .: "hasActiveExtension"
                          <*> v .: "extended"
                          <*> v .: "late"
                          <*> v .: "openEnded"
                          <*> v .: "opened"
                          <*> v .: "closed"
                          <*> v .: "openDate"
                          <*> v .:? "closeDate"
                          <*> v .:? "submission"
                          <*> v .:? "feedback"
                          <*> v .:? "extension"

data AssignmentInformation = AssignmentInformation {
    enrolledAssignments :: [StudentAssignment],
    historicAssignments :: [StudentAssignment]
} deriving Show

instance FromJSON AssignmentInformation where
    parseJSON = withObject "AssignmentInformation" $ \v ->
        AssignmentInformation <$> v .: "enrolledAssignments"
                              <*> v .: "historicAssignments"

--------------------------------------------------------------------------------

lateSubmissions :: [StudentAssignment] -> [StudentAssignment]
lateSubmissions = filter $ \StudentAssignment{..} -> case studentAssignmentSubmission of
    Nothing -> False
    Just (StudentAssignmentSubmission {..}) -> studentAssignmentSubmissionLate

--------------------------------------------------------------------------------
