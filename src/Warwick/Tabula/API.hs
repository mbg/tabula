--------------------------------------------------------------------------------
-- Haskell bindings for the Tabula API                                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Tabula.API where

--------------------------------------------------------------------------------

import Data.Aeson
import qualified Data.HashMap.Lazy as HM
import Data.ByteString as BS
import Data.UUID.Types
import Data.Proxy
import Data.Text (Text)

import Servant.API

import Warwick.Tabula.Types
import Warwick.Tabula.Attachment
import Warwick.Tabula.Coursework
import Warwick.Tabula.Job
import Warwick.Tabula.Member
import Warwick.Tabula.Payload
import Warwick.Tabula.Relationship
import Warwick.Tabula.StudentAssignment

--------------------------------------------------------------------------------

-- | Represents Tabula responses with payloads of type a.
data TabulaResponse a
    = TabulaOK {
        tabulaStatus  :: String,
        tabulaOffset  :: Maybe Int,
        tabulaLimit   :: Maybe Int,
        tabulaTotal   :: Maybe Int,
        tabulaData    :: a
    } deriving (Show)

data TabulaAssignmentResponse
    = TabulaAssignmentOK {
        tabulaAssignmentStatus :: String,
        tabulaAssignmentData   :: AssignmentInformation
    } deriving Show

instance (HasPayload a, FromJSON a) => FromJSON (TabulaResponse a) where
    parseJSON = withObject "TabulaResponse" $ \v -> do
        s <- v .: "success"
        if s
        then TabulaOK <$> v .: "status"
                      <*> v .:? "offset"
                      <*> v .:? "limit"
                      <*> v .:? "total"
                      <*> payload v
        else fail "200 OK, but success is not true (this should not happen)"

instance FromJSON TabulaAssignmentResponse where
    parseJSON = withObject "TabulaAssignmentResponse" $ \v -> do
        s <- v .: "success"
        if s
        then TabulaAssignmentOK <$> v .: "status"
                                <*> parseJSON (Object v)
        else fail "200 OK, but success is not true (this should not happen)"

type TabulaAuth = BasicAuth "" ()

type AdminAPI = 
      TabulaAuth :>
      "module" :>
      Capture "moduleCode" ModuleCode :>
      Get '[JSON] (TabulaResponse Module)
 :<|> TabulaAuth :>
      "department" :>
      Capture "departmentCode" Text :>
      Get '[JSON] (TabulaResponse Department)

-- | Represents the coursework part of Tabula's API as a type.
type CourseworkAPI =
      TabulaAuth :> 
      "module" :> 
      Capture "moduleCode" ModuleCode :> 
      "assignments" :> 
      QueryParam "academicYear" String :> 
      Get '[JSON] (TabulaResponse [Assignment])
 :<|> TabulaAuth :> 
      "module" :> 
      Capture "moduleCode" ModuleCode :> 
      "assignments" :> 
      Capture "assignmentID" UUID :> 
      QueryParam "filter" String :> 
      Get '[JSON] (TabulaResponse Assignment)
 :<|> TabulaAuth :> 
      "module" :> 
      Capture "moduleCode" ModuleCode :> 
      "assignments" :> 
      Capture "assignmentId" UUID :> 
      "submissions" :> 
      Get '[JSON] (TabulaResponse (HM.HashMap String (Maybe Submission)))
 :<|> TabulaAuth :>
      "module" :>
      Capture "moduleCode" ModuleCode :>
      "assignments" :>
      Capture "assignmentId" UUID :> 
      "marks" :>
      ReqBody '[JSON] Marks :>
      Post '[JSON] (TabulaResponse None)
 :<|> TabulaAuth :> 
      "attachments" :> 
      QueryParam "filename" String :> 
      ReqBody '[OctetStream] BS.ByteString :> 
      Post '[JSON] (TabulaResponse Attachment)
 :<|> TabulaAuth :> 
      "job" :> 
      Capture "jobID" UUID :> 
      Get '[JSON] (TabulaResponse JobInstance)

type SmallGroupAPI = 
      TabulaAuth :> 
      "module" :>
      Capture "moduleCode" ModuleCode :> 
      "groups" :>
      QueryParam "academicYear" Text :>
      Get '[JSON] (TabulaResponse [SmallGroupSet])
 :<|> TabulaAuth :>
      "module" :>
      Capture "moduleCode" ModuleCode :>
      "groups" :>
      Capture "smallGroupSetId" Text :> 
      "allocations" :>
      Get '[JSON] (TabulaResponse SmallGroupAllocations)
 :<|> TabulaAuth :>
      "groups" :>
      Capture "smallGroupId" Text :> 
      "attendance" :>
      Get '[JSON] (TabulaResponse SmallGroupAttendanceResponse)

-- | Represents the membership part of Tabula's API as a type.
type MemberAPI =
      TabulaAuth :> 
      "member" :> 
      Capture "userID" String :> 
      QueryParam "fields" String :> 
      Get '[JSON] (TabulaResponse Member)
 :<|> TabulaAuth :> 
      "member" :> 
      Capture "userID" String :> 
      "relationships" :> 
      Get '[JSON] (TabulaResponse [Relationship])
 :<|> TabulaAuth :> 
      "member" :> 
      Capture "userID" String :> 
      "assignments" :> 
      QueryParam "academicYear" Text :>
      Get '[JSON] TabulaAssignmentResponse
 :<|> TabulaAuth :>
      "members" :> 
      QueryParams "department" Text :>
      QueryParam "fields" Text :>
      QueryParam "offset" Int :>
      QueryParam "limit" Int :>
      QueryParam "courseTypes" Text :>
      QueryParam "routes" Text :>
      QueryParam "courses" Text :>
      QueryParam "modesOfAttendance" Text :> 
      QueryParam "yearsOfStudy" Text :> 
      QueryParam "levelCodes" Text :>
      QueryParam "sprStatuses" Text :> 
      QueryParam "modules" Text :>
      QueryParam "hallsOfResidence" Text :>
      Get '[JSON] (TabulaResponse [Member])
 :<|> TabulaAuth :>
      "member" :>
      Capture "universityId" Text :> 
      "attendance" :> 
      Capture "academicYear" Text :>
      Get '[JSON] (TabulaResponse MemberAttendance)

-- | Represents the timetabling part of Tabula's API as a type.
type TimetableAPI =
      "termdates" :> 
      Get '[JSON] (TabulaResponse [Term])
 :<|> "termdates" :> 
      Capture "academicYear" Text :> 
      Get '[JSON] (TabulaResponse [Term])
 :<|> "termdates" :> 
      "weeks" :>
      QueryParam "numberingSystem" NumberingSystem :>
      Get '[JSON] (TabulaResponse [Week])
 :<|> "termdates" :> 
      Capture "academicYear" Text :> 
      "weeks" :>
      QueryParam "numberingSystem" NumberingSystem :>
      Get '[JSON] (TabulaResponse [Week])
 :<|> "holidaydates" :>
      Get '[JSON] (TabulaResponse [Holiday])

-- | Represents the Tabula API as a type.
type TabulaAPI = 
    AdminAPI :<|>
    CourseworkAPI :<|> 
    SmallGroupAPI :<|>
    MemberAPI :<|>
    TimetableAPI

-- | A proxy value for the `API` type.
tabula :: Proxy TabulaAPI
tabula = Proxy

--------------------------------------------------------------------------------
