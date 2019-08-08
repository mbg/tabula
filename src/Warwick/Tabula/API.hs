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
import Warwick.Tabula.Error
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
      "attachments" :> 
      QueryParam "filename" String :> 
      ReqBody '[OctetStream] BS.ByteString :> 
      Post '[JSON] (TabulaResponse Attachment)
 :<|> TabulaAuth :> 
      "job" :> 
      Capture "jobID" UUID :> 
      Get '[JSON] (TabulaResponse JobInstance)

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
      Get '[JSON] TabulaAssignmentResponse

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
    CourseworkAPI :<|> 
    MemberAPI :<|>
    TimetableAPI

-- | A proxy value for the `API` type.
tabula :: Proxy TabulaAPI
tabula = Proxy

--------------------------------------------------------------------------------
