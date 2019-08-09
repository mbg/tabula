--------------------------------------------------------------------------------
-- Haskell bindings for the Tabula API                                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Tabula.Member where

--------------------------------------------------------------------------------

import Data.Aeson

import Warwick.Tabula.Types
import Warwick.Tabula.Payload.StudentCourseDetails

--------------------------------------------------------------------------------

data MemberField
    = MemberUniversityID
    | MemberFullName

instance Show MemberField where
    show MemberUniversityID = "member.universityId"
    show MemberFullName = "member.fullName"

-- | Represents a member object.
data Member = Member {
    memberID            :: Maybe String,
    memberUserID        :: Maybe String,
    memberFirstName     :: Maybe String,
    memberFullFirstName :: Maybe String,
    memberLastName      :: Maybe String,
    memberEmail         :: Maybe String,
    memberUserType      :: Maybe String,
    memberFullName      :: Maybe String,
    memberOfficialName  :: Maybe String,
    memberHomeEmail     :: Maybe String,
    memberTitle         :: Maybe String,
    memberGender        :: Maybe String,
    memberInUse         :: Maybe String,
    memberJobTitle      :: Maybe String,
    memberPhoneNumber   :: Maybe String,
    -- TODO: some private details fields here
    memberGroupName        :: Maybe String,
    -- TODO: some more fields here
    memberInactivationDate :: Maybe Date,
    -- TODO: some more fields here
    memberStudentCourseDetails :: Maybe [StudentCourseDetails]
}

instance FromJSON Member where
    parseJSON = withObject "member" $ \v ->
        Member <$> v .:? "universityId"
               <*> v .:? "userId"
               <*> v .:? "firstName"
               <*> v .:? "fullFirstName"
               <*> v .:? "lastName"
               <*> v .:? "email"
               <*> v .:? "userType"
               <*> v .:? "fullName"
               <*> v .:? "officialName"
               <*> v .:? "homeEmail"
               <*> v .:? "title"
               <*> v .:? "gender"
               <*> v .:? "inUseFlag"
               <*> v .:? "jobTitle"
               <*> v .:? "phoneNumber"
               <*> v .:? "groupName"
               <*> v .:? "inactivationDate"
               <*> v .:? "studentCourseDetails"

instance HasPayload Member where
    payloadFieldName _ = "member"

--------------------------------------------------------------------------------
