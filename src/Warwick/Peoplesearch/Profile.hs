--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Peoplesearch.Profile where 

--------------------------------------------------------------------------------

import Data.Aeson 
import Data.Text
import Data.UUID.Types

import Warwick.Common

--------------------------------------------------------------------------------

-- | Represents a department.
data Department = Department {
    -- | The department code.
    dCode :: Text,
    -- | The short name of the department.
    dVeryShortName :: Text, 
    -- | The name of the department.
    dName :: Text,
    -- | The UUID of the department.
    dID :: UUID,
    -- | A URL to the homepage of the department.
    dHomepage :: Maybe Text, 
    -- | The long name of the department.
    dLongName :: Text
} deriving Show

instance FromJSON Department where 
    parseJSON = withObject "Department" $ \obj ->
        Department <$> obj .: "code"
                   <*> obj .: "veryShortName"
                   <*> obj .: "name"
                   <*> obj .: "id"
                   <*> obj .: "homepage"
                   <*> obj .: "longName"

-- | Information about a user's membership in the university.
data MembershipDetails = MembershipDetails {
    -- | Their university ID.
    mdUniversityID :: Text,
    -- | Their external email address, if any.
    mdExternalEmail :: Maybe Text,
    -- | Their preferred surname.
    mdPreferredSurname :: Text,
    -- | Their end date at the university.
    mdEndDate :: Date,
    -- | Their preferred first name.
    mdPreferredFirstName :: Text,
    -- | The last time their profile was modified.
    mdLastModified :: DateTime,
    -- | Their UUID.
    mdID :: UUID,
    -- | Their title, if available.
    mdTitle :: Maybe Text,
    -- | Their start date at the university.
    mdStartDate :: Date,
    -- | Their university email address.
    mdEmail :: Text,
    -- | The code of the department they belong to.
    mdDeptCode :: Text,
    -- | The URL of their homepage, if available.
    mdHomepage :: Maybe Text
} deriving Show

instance FromJSON MembershipDetails where 
    parseJSON = withObject "MembershipDetails" $ \obj ->
        MembershipDetails <$> obj .: "universityId"
                          <*> obj .: "externalEmail"
                          <*> obj .: "preferredSurname"
                          <*> obj .: "endDate"
                          <*> obj .: "preferredFirstName"
                          <*> obj .: "lastModified"
                          <*> obj .: "id"
                          <*> obj .: "title"
                          <*> obj .: "startDate"
                          <*> obj .: "email"
                          <*> obj .: "deptCode"
                          <*> obj .: "homepage"

-- | Represents a user's profile.
data Profile = Profile {
    pExtensionNumberWithExternal :: Maybe Text,
    pTargetGroup :: Text,
    pMobileVisibility :: Text, 
    pExternalNumber :: Maybe Text,
    pMobileNumber :: Maybe Text, 
    pCustomFieldDetails :: [Text],
    pType :: Text, 
    pPreferredNumber :: Maybe Text,
    pBuilding :: Maybe Text,
    pScore :: Double, 
    pJobFunction :: Maybe Text,
    pID :: UUID,
    pDepartment :: Department,
    pSequenceNumber :: Int,
    pMatchFields :: Text,
    -- TODO: no idea what the format of these is
    -- pDeptRoleSubCategories :: [()],
    -- pCustomFieldNames :: Maybe (),
    -- pSkillsAndExperience :: Maybe (),
    pRoom :: Maybe Text, 
    pPrimaryAccount :: Bool,
    pMembershipDetails :: MembershipDetails,
    pExtensionNumber :: Maybe Text, 
    pJobDescription :: Maybe Text, 
    pLastModified :: DateTime, 
    pPhotoVisibility :: Text
} deriving Show

instance FromJSON Profile where 
    parseJSON = withObject "Profile" $ \obj ->
        Profile <$> obj .:? "extensionNumberWithExternal"
                <*> obj .: "targetGroup"
                <*> obj .: "mobileVisibility"
                <*> obj .: "externalNumber"
                <*> obj .: "mobileNumber"
                <*> obj .: "customFieldDetails"
                <*> obj .: "type"
                <*> obj .: "preferredNumber"
                <*> obj .: "building"
                <*> obj .: "score"
                <*> obj .: "jobFunction"
                <*> obj .: "id"
                <*> obj .: "department"
                <*> obj .: "sequenceNumber"
                <*> obj .: "matchFields"
                -- <*> obj .: "deptRoleSubCategories"
                -- <*> obj .: "customFieldNames"
                -- <*> obj .: "skillsAndExperience"
                <*> obj .: "room"
                <*> obj .: "primaryAccount"
                <*> obj .: "membershipDetails"
                <*> obj .: "extensionNumber"
                <*> obj .: "jobDescription"
                <*> obj .: "lastModified"
                <*> obj .: "photoVisibility"

--------------------------------------------------------------------------------
