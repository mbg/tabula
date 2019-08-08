--------------------------------------------------------------------------------
-- Haskell bindings for the Tabula API                                        --
-- Copyright 2018 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Tabula.Internal where

--------------------------------------------------------------------------------

import qualified Data.HashMap.Lazy as HM
import qualified Data.ByteString as BS

import Servant.API
import Servant.Client

import Warwick.Tabula.Types
import Warwick.Tabula.API
import Warwick.Tabula.Attachment
import Warwick.Tabula.Coursework
import Warwick.Tabula.Job
import Warwick.Tabula.Member
import Warwick.Tabula.Relationship
import Warwick.Tabula.StudentAssignment
import Warwick.Tabula.Payload.Term

--------------------------------------------------------------------------------

listAssignments :: BasicAuthData -> ModuleCode -> Maybe AcademicYear -> ClientM (TabulaResponse [Assignment])
retrieveAssignment :: BasicAuthData -> ModuleCode -> UUID -> Maybe String -> ClientM (TabulaResponse Assignment)
listSubmissions :: BasicAuthData -> ModuleCode -> UUID -> ClientM (TabulaResponse (HM.HashMap String (Maybe Submission)))

uploadAttachment :: BasicAuthData -> Maybe String -> BS.ByteString -> ClientM (TabulaResponse Attachment)

retrieveJob :: BasicAuthData -> UUID -> ClientM (TabulaResponse JobInstance)

retrieveMember :: BasicAuthData -> String -> Maybe String -> ClientM (TabulaResponse Member)
listRelationships :: BasicAuthData -> String -> ClientM (TabulaResponse [Relationship])
personAssignments :: BasicAuthData -> String -> ClientM TabulaAssignmentResponse


courseworkAPI :<|> memberAPI :<|> timetableAPI = client tabula

listAssignments :<|>
    retrieveAssignment :<|>
    listSubmissions :<|>
    uploadAttachment :<|>
    retrieveJob = courseworkAPI

retrieveMember :<|>
    listRelationships :<|>
    personAssignments = memberAPI

retrieveTermDates ::
    BasicAuthData -> 
    Maybe String -> 
    ClientM (TabulaResponse [Term])

retrieveTermDates = timetableAPI

--------------------------------------------------------------------------------
