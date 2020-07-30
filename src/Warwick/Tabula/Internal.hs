--------------------------------------------------------------------------------
-- Haskell bindings for the Tabula API                                        --
-- Copyright 2018 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Tabula.Internal where

--------------------------------------------------------------------------------

import qualified Data.HashMap.Lazy as HM
import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Time

import Servant.API
import Servant.Client

import Warwick.Tabula.Types
import Warwick.Tabula.API
import Warwick.Tabula.Attachment
import Warwick.Tabula.Coursework
import Warwick.Tabula.Job
import Warwick.Tabula.Member
import Warwick.Tabula.Payload
import Warwick.Tabula.StudentAssignment

--------------------------------------------------------------------------------

adminAPI :<|> courseworkAPI :<|> smallGroupAPI :<|> memberAPI :<|> timetableAPI = client tabula

--------------------------------------------------------------------------------

retrieveModule :: BasicAuthData -> ModuleCode -> ClientM (TabulaResponse Module)
listRegisteredUsercodes :: BasicAuthData -> ModuleCode -> ClientM (TabulaResponse UsercodeList)
listRegisteredUsercodesIn :: BasicAuthData -> ModuleCode -> Text -> ClientM (TabulaResponse UsercodeList)
listDepartments :: BasicAuthData -> ClientM (TabulaResponse [Department])
retrieveDepartment :: BasicAuthData -> Text -> ClientM (TabulaResponse Department)
listDepartmentModules :: BasicAuthData -> Text -> ClientM (TabulaResponse [Module])

retrieveModule :<|> 
    listRegisteredUsercodes :<|>
    listRegisteredUsercodesIn :<|>
    listDepartments :<|> 
    retrieveDepartment :<|> 
    listDepartmentModules = adminAPI

--------------------------------------------------------------------------------

listAssignments :: BasicAuthData -> ModuleCode -> Maybe AcademicYear -> ClientM (TabulaResponse [Assignment])
retrieveAssignment :: BasicAuthData -> ModuleCode -> UUID -> Maybe String -> ClientM (TabulaResponse Assignment)
listSubmissions :: BasicAuthData -> ModuleCode -> UUID -> ClientM (TabulaResponse (HM.HashMap String (Maybe Submission)))
postMarks :: BasicAuthData -> ModuleCode -> UUID -> Marks -> ClientM (TabulaResponse None)

uploadAttachment :: BasicAuthData -> Maybe String -> BS.ByteString -> ClientM (TabulaResponse Attachment)

retrieveJob :: BasicAuthData -> UUID -> ClientM (TabulaResponse JobInstance)

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

retrieveSmallGroupAllocations ::
    BasicAuthData ->
    ModuleCode ->
    Text ->
    ClientM (TabulaResponse SmallGroupAllocations)

retrieveSmallGroupAttendance :: 
    BasicAuthData -> 
    Text -> 
    ClientM (TabulaResponse SmallGroupAttendanceResponse)

listSmallGroupSets :<|> 
    retrieveSmallGroupAllocations :<|>
    retrieveSmallGroupAttendance = smallGroupAPI

--------------------------------------------------------------------------------

retrieveMember :: 
    BasicAuthData -> 
    String -> 
    Maybe String -> 
    ClientM (TabulaResponse Member)

personAssignments :: 
    BasicAuthData -> 
    String -> 
    Maybe Text -> 
    ClientM TabulaAssignmentResponse

retrieveMembers :: BasicAuthData 
                -> [Text] 
                -> Maybe Text 
                -> ClientM (TabulaResponse (HM.HashMap Text Member))

listMembers ::
    BasicAuthData ->
    [Text] ->
    Maybe AcademicYear ->
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

listRelationships :: 
    BasicAuthData -> 
    Text -> 
    Maybe Text ->
    ClientM (TabulaResponse [Relationship])

listRelationshipTypes :: 
    BasicAuthData -> 
    ClientM (TabulaResponse [RelationshipType])

listAgents :: 
    BasicAuthData -> 
    Text -> 
    Text ->
    ClientM (TabulaResponse AgentList)

retrieveMember :<|>
    retrieveMembers :<|>
    listRelationships :<|>
    personAssignments :<|>
    listMembers :<|>
    retrieveAttendance :<|>
    listRelationshipTypes :<|>
    listAgents = memberAPI

--------------------------------------------------------------------------------

retrieveMemberEvents ::
    BasicAuthData ->
    Text -> 
    Maybe Text -> 
    Maybe Day -> 
    Maybe Day -> 
    ClientM (TabulaResponse [EventOccurrence])

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

retrieveMemberEvents :<|>
    retrieveTermDates :<|>
    retrieveTermDatesFor :<|>
    retrieveTermWeeks :<|>
    retrieveTermWeeksFor :<|>
    retrieveHolidays = timetableAPI

--------------------------------------------------------------------------------
