--------------------------------------------------------------------------------
-- Haskell bindings for the Tabula API                                        --
-- Copyright 2018 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Tabula.Internal where

--------------------------------------------------------------------------------

import qualified Data.HashMap.Lazy as HM
import qualified Data.ByteString as BS
import Data.Text (Text)

import Servant.API
import Servant.Client

import Warwick.Tabula.Types
import Warwick.Tabula.API
import Warwick.Tabula.Attachment
import Warwick.Tabula.Coursework
import Warwick.Tabula.Job
import Warwick.Tabula.Member
import Warwick.Tabula.Payload
import Warwick.Tabula.Relationship
import Warwick.Tabula.StudentAssignment

--------------------------------------------------------------------------------

adminAPI :<|> courseworkAPI :<|> smallGroupAPI :<|> memberAPI :<|> timetableAPI = client tabula

--------------------------------------------------------------------------------

retrieveModule :: BasicAuthData -> ModuleCode -> ClientM (TabulaResponse Module)
retrieveDepartment :: BasicAuthData -> Text -> ClientM (TabulaResponse Department)

retrieveModule :<|> retrieveDepartment = adminAPI

--------------------------------------------------------------------------------

listAssignments :: BasicAuthData -> ModuleCode -> Maybe AcademicYear -> ClientM (TabulaResponse [Assignment])
retrieveAssignment :: BasicAuthData -> ModuleCode -> UUID -> Maybe String -> ClientM (TabulaResponse Assignment)
listSubmissions :: BasicAuthData -> ModuleCode -> UUID -> ClientM (TabulaResponse (HM.HashMap String (Maybe Submission)))
postMarks :: BasicAuthData -> ModuleCode -> UUID -> Marks -> ClientM (TabulaResponse None)

uploadAttachment :: BasicAuthData -> Maybe String -> BS.ByteString -> ClientM (TabulaResponse Attachment)

retrieveJob :: BasicAuthData -> UUID -> ClientM (TabulaResponse JobInstance)

retrieveMember :: BasicAuthData -> String -> Maybe String -> ClientM (TabulaResponse Member)
listRelationships :: BasicAuthData -> String -> ClientM (TabulaResponse [Relationship])
personAssignments :: BasicAuthData -> String -> Maybe Text -> ClientM TabulaAssignmentResponse

listAssignments :<|>
    retrieveAssignment :<|>
    listSubmissions :<|>
    postMarks :<|>
    uploadAttachment :<|>
    retrieveJob = courseworkAPI

--------------------------------------------------------------------------------

listSmallGroupSets :: 
    BasicAuthData -> 
    ModuleCode -> 
    Maybe Text -> 
    ClientM (TabulaResponse [SmallGroupSet])
    
listSmallGroupSets = smallGroupAPI

--------------------------------------------------------------------------------

listMembers ::
    BasicAuthData ->
    [Text] ->
    Maybe Text ->
    Maybe Int ->
    Maybe Int ->
    Maybe Text ->
    Maybe Text ->
    Maybe Text ->
    Maybe Text ->
    Maybe Text ->
    Maybe Text ->
    Maybe Text ->
    Maybe Text ->
    Maybe Text ->
    ClientM (TabulaResponse [Member])

retrieveAttendance :: 
    BasicAuthData -> 
    Text ->
    Text ->
    ClientM (TabulaResponse MemberAttendance)

retrieveMember :<|>
    listRelationships :<|>
    personAssignments :<|>
    listMembers :<|>
    retrieveAttendance = memberAPI

--------------------------------------------------------------------------------

retrieveTermDates ::
    ClientM (TabulaResponse [Term])

retrieveTermDatesFor ::
    Text -> 
    ClientM (TabulaResponse [Term])

retrieveTermWeeks ::
    Maybe NumberingSystem ->
    ClientM (TabulaResponse [Week])

retrieveTermWeeksFor ::
    Text ->
    Maybe NumberingSystem ->
    ClientM (TabulaResponse [Week])

retrieveHolidays ::
    ClientM (TabulaResponse [Holiday])

retrieveTermDates :<|>
    retrieveTermDatesFor :<|>
    retrieveTermWeeks :<|>
    retrieveTermWeeksFor :<|>
    retrieveHolidays = timetableAPI

--------------------------------------------------------------------------------
