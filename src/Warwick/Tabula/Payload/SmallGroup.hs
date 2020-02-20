--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Tabula.Payload.SmallGroup where 

--------------------------------------------------------------------------------

import Data.Aeson
import Data.Text
import qualified Data.Map as M

import Warwick.Common
import Warwick.Tabula.Types
import Warwick.Tabula.Attachment
import Warwick.Tabula.Payload.Module
import Warwick.Tabula.Payload.Note

--------------------------------------------------------------------------------
    
data SmallGroupFormat
    = Seminar 
    | Lab 
    | Tutorial
    | Project
    | Example
    | Workshop
    | Lecture 
    | Meeting
    | Exam 
    deriving (Eq, Show)

instance FromJSON SmallGroupFormat where 
    parseJSON (String "seminar") = pure Seminar
    parseJSON (String "lab") = pure Lab
    parseJSON (String "tutorial") = pure Tutorial
    parseJSON (String "project") = pure Project
    parseJSON (String "example") = pure Example
    parseJSON (String "workshop") = pure Workshop
    parseJSON (String "lecture") = pure Lecture
    parseJSON (String "meeting") = pure Meeting
    parseJSON (String "exam") = pure Exam
    parseJSON _ = fail "Not a valid SmallGroupFormat"

--------------------------------------------------------------------------------

data SmallGroupSet = SmallGroupSet {
    sgsId :: Text,
    sgsArchived :: Maybe Bool,
    sgsAcademicYear :: Maybe Text, 
    sgsName :: Text,
    sgsFormat :: SmallGroupFormat,
    sgsModule :: Module

} deriving (Eq, Show)

instance FromJSON SmallGroupSet where 
    parseJSON = withObject "SmallGroupSet" $ \obj -> 
        SmallGroupSet <$> obj .: "id"
                      <*> obj .:? "archived"
                      <*> obj .:? "academicYear"
                      <*> obj .: "name"
                      <*> obj .: "format"
                      <*> obj .: "module"

instance HasPayload [SmallGroupSet] where 
    payloadFieldName _ = "groups"

--------------------------------------------------------------------------------

data Tutor = Tutor {
    tUserId :: Text,
    tUniversityId :: Maybe Text
} deriving (Eq, Show)

instance FromJSON Tutor where 
    parseJSON = withObject "Tutor" $ \obj ->
        Tutor <$> obj .: "userId" <*> obj .:? "universityId"

--------------------------------------------------------------------------------

data Location = Location {
    lName :: Text,
    lId :: Maybe Text 
} deriving (Eq, Show)

instance FromJSON Location where 
    parseJSON = withObject "Location" $ \obj -> 
        Location <$> obj .: "name" <*> obj .:? "locationId"

--------------------------------------------------------------------------------

data EventRef = EventRef {
    erId :: Text, 
    erWeek :: Int
} deriving (Eq, Show)

instance FromJSON EventRef where 
    parseJSON = withObject "EventRef" $ \obj ->
        EventRef <$> obj .: "event"
                 <*> obj .: "week"

data Event = Event {
    eId :: Text,
    eTitle :: Text,
    eDay :: Text,
    eStartTime :: Text,
    eEndTime :: Text,
    eLocation :: Maybe Location,
    eTutors :: [Tutor]
} deriving (Eq, Show)

instance FromJSON Event where 
    parseJSON = withObject "Event" $ \obj ->
        Event <$> obj .: "id"
              <*> obj .: "title"
              <*> obj .: "day"
              <*> obj .: "startTime"
              <*> obj .: "endTime"
              <*> obj .:? "location"
              <*> obj .: "tutors"

--------------------------------------------------------------------------------

data SmallGroup = SmallGroup {
    sgId :: Text,
    sgName :: Text
} deriving (Eq, Show)

instance FromJSON SmallGroup where 
    parseJSON = withObject "SmallGroup" $ \obj ->
        SmallGroup <$> obj .: "id"
                   <*> obj .: "name"

--------------------------------------------------------------------------------

data EventState
    = AttendedEvent 
    | MissedAuthorisedEvent
    | MissedUnauthorisedEvent
    | NotRecordedEvent
    | LateEvent
    | NotExpectedEvent
    | Other Text
    deriving (Eq, Show)

instance FromJSON EventState where 
    parseJSON (String "Attended") = pure AttendedEvent
    parseJSON (String "MissedAuthorised") = pure MissedAuthorisedEvent
    parseJSON (String "MissedUnauthorised") = pure MissedUnauthorisedEvent
    parseJSON (String "NotRecorded") = pure NotRecordedEvent
    parseJSON (String "Late") = pure LateEvent
    parseJSON (String "NotExpected") = pure NotExpectedEvent
    parseJSON (String other) = pure $ Other other
    parseJSON _ = fail "Not a valid EventState"

--------------------------------------------------------------------------------

data EventAttendance = EventAttendance {
    eaEvent :: Event,
    eaWeekNumber :: Int, 
    eaState :: EventState,
    eaNote :: Maybe AttendanceNote
} deriving (Eq, Show)

instance FromJSON EventAttendance where 
    parseJSON = withObject "EventAttendance" $ \obj ->
        EventAttendance <$> obj .: "event"
                        <*> obj .: "weekNumber"
                        <*> obj .: "state"
                        <*> obj .:? "note"

--------------------------------------------------------------------------------

data SmallGroupAttendance = SmallGroupAttendance {
    sgaGroup :: SmallGroup,
    sgaAttendance :: [EventAttendance]
} deriving (Eq, Show)

instance FromJSON SmallGroupAttendance where 
    parseJSON = withObject "SmallGroupAttendance" $ \obj ->
        SmallGroupAttendance <$> obj .: "group"
                             <*> obj .: "attendance"

--------------------------------------------------------------------------------

data SmallGroupSetAttendance = SmallGroupSetAttendance {
    sgsaGroupSet :: SmallGroupSet,
    sgsaGroups :: [SmallGroupAttendance]
} deriving (Eq, Show)

instance FromJSON SmallGroupSetAttendance where 
    parseJSON = withObject "SmallGroupSetAttendance" $ \obj ->
        SmallGroupSetAttendance <$> obj .: "groupSet"
                                <*> obj .: "groups"

--------------------------------------------------------------------------------

newtype SmallGroupAllocations = SGT (M.Map Text [Text])
    deriving (Eq, Show)

instance FromJSON SmallGroupAllocations where 
    parseJSON v = SGT <$> parseJSON v

instance HasPayload SmallGroupAllocations where 
    payloadFieldName _ = "allocations" 

--------------------------------------------------------------------------------

data StudentEventAttendance = StudentEventAttendance {
    seaEventRef :: EventRef, 
    seaStatus :: EventAttendanceStatus
} deriving (Eq, Show)

instance FromJSON StudentEventAttendance where
    parseJSON = withObject "StudentEventAttendance" $ \obj ->
        StudentEventAttendance <$> obj .: "event"
                               <*> obj .: "status"

data EventAttendanceStatus = EventAttendanceStatus {
    easState :: Text,
    easDetails :: Maybe SmallGroupEventAttendance,
    easNote :: Maybe AttendanceNote
} deriving (Eq, Show)

instance FromJSON EventAttendanceStatus where 
    parseJSON = withObject "EventAttendanceStatus" $ \obj ->
        EventAttendanceStatus <$> obj .: "state"
                              <*> obj .:? "details"
                              <*> obj .:? "note"

data SmallGroupEventAttendance = SmallGroupEventAttendance {
    sgeaUpdatedBy :: Text,
    sgeaUpdatedDate :: Date,
    sgeaJoinedOn :: Maybe Date,
    sgeaExpectedToAttend :: Bool,
    sgeaAddedManually :: Bool,
    sgeaReplacesAttendance :: Maybe SmallGroupEventAttendance,
    sgeaReplacedBy :: [SmallGroupEventAttendance]
} deriving (Eq, Show)

instance FromJSON SmallGroupEventAttendance where 
    parseJSON = withObject "SmallGroupEventAttendance" $ \obj -> 
        SmallGroupEventAttendance <$> obj .: "updatedBy"
                                  <*> obj .: "updatedDate"
                                  <*> obj .:? "joinedOn"
                                  <*> obj .: "expectedToAttend"
                                  <*> obj .: "addedManually"
                                  <*> obj .:? "replacesAttendance"
                                  <*> obj .: "replacedBy"

data StudentAttendance = StudentAttendance {
    saUniversityID :: Text, 
    saEvents :: [StudentEventAttendance]
} deriving (Eq, Show)

instance FromJSON StudentAttendance where 
    parseJSON = withObject "StudentAttendance" $ \obj ->
        StudentAttendance <$> obj .: "student"
                          <*> obj .: "events"

data SmallGroupAttendanceResponse = SmallGroupAttendanceResponse {
    sgarEvents :: M.Map Text Event,
    sgarInstances :: M.Map Text [Int],
    sgarAttendance :: [StudentAttendance]
} deriving (Eq, Show)

instance FromJSON SmallGroupAttendanceResponse where 
    parseJSON = withObject "SmallGroupAttendanceResponse" $ \obj -> 
        SmallGroupAttendanceResponse <$> obj .: "events"
                                     <*> obj .: "instances"
                                     <*> obj .: "attendance"

instance HasPayload SmallGroupAttendanceResponse where 
    payloadFieldName _ = "attendance"

--------------------------------------------------------------------------------