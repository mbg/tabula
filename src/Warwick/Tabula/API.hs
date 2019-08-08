--------------------------------------------------------------------------------
-- Haskell bindings for the Tabula API                                        --
-- Copyright 2018 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Tabula.API where

--------------------------------------------------------------------------------

import Data.Aeson
import qualified Data.HashMap.Lazy as HM
import Data.ByteString as BS
import Data.UUID.Types
import Data.Proxy

import Servant.API

import Warwick.Tabula.Types
import Warwick.Tabula.Error
import Warwick.Tabula.Attachment
import Warwick.Tabula.Coursework
import Warwick.Tabula.Job
import Warwick.Tabula.Member
import Warwick.Tabula.Relationship
import Warwick.Tabula.StudentAssignment
import Warwick.Tabula.Payload.Term

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

type CourseworkAPI =
      TabulaAuth :> "module" :> Capture "moduleCode" ModuleCode :> "assignments" :> QueryParam "academicYear" String :> Get '[JSON] (TabulaResponse [Assignment])
 :<|> TabulaAuth :> "module" :> Capture "moduleCode" ModuleCode :> "assignments" :> Capture "assignmentID" UUID :> QueryParam "filter" String :> Get '[JSON] (TabulaResponse Assignment)
 :<|> TabulaAuth :> "module" :> Capture "moduleCode" ModuleCode :> "assignments" :> Capture "assignmentId" UUID :> "submissions" :> Get '[JSON] (TabulaResponse (HM.HashMap String (Maybe Submission)))
 :<|> TabulaAuth :> "attachments" :> QueryParam "filename" String :> ReqBody '[OctetStream] BS.ByteString :> Post '[JSON] (TabulaResponse Attachment)
 :<|> TabulaAuth :> "job" :> Capture "jobID" UUID :> Get '[JSON] (TabulaResponse JobInstance)

type TabulaMemberAPI =
      TabulaAuth :> "member" :> Capture "userID" String :> QueryParam "fields" String :> Get '[JSON] (TabulaResponse Member)
 :<|> TabulaAuth :> "member" :> Capture "userID" String :> "relationships" :> Get '[JSON] (TabulaResponse [Relationship])
 :<|> TabulaAuth :> "member" :> Capture "userID" String :> "assignments" :> Get '[JSON] TabulaAssignmentResponse

type TabulaTimetableAPI =
    TabulaAuth :> 
    "termdates" :> 
    QueryParam "academicYear" String :> 
    Get '[JSON] (TabulaResponse [Term])

type TabulaAPI = 
    CourseworkAPI :<|> 
    TabulaMemberAPI :<|>
    TabulaTimetableAPI

type API = TabulaAPI

tabula :: Proxy API
tabula = Proxy

--------------------------------------------------------------------------------
