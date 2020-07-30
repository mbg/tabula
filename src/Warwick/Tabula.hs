--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

module Warwick.Tabula (
    module Warwick.Common,
    module Warwick.Config,
    module Warwick.Tabula.Coursework,

    Tabula,
    TabulaErr(..),

    TabulaInstance(..),

    ModuleCode(..),
    AssignmentID(..),

    TabulaResponse(..),
    TabulaAssignmentResponse(..),

    withTabula,

    -- * Administration & information API
    retrieveModule,
    listRegisteredUsercodes,
    listRegisteredUsercodesIn,
    listRegisteredUniversityIds,
    listRegisteredUniversityIdsIn,
    listDepartments,
    retrieveDepartment,
    listDepartmentModules,

    -- * Coursework API
    listAssignments,
    retrieveAssignment,
    listSubmissions,
    postMarks,

    TabulaDownloadCallbacks(..),
    downloadSubmission,
    downloadSubmissionWithCallbacks,

    -- * Small group teaching API
    listSmallGroupSets,
    retrieveSmallGroupAllocations,
    retrieveSmallGroupAttendance,

    -- * Profiles API
    retrieveMember,
    retrieveMembers,
    listRelationships,
    personAssignments,
    listMembers,
    retrieveAttendance,
    listRelationshipTypes,
    listAgents,
    
    -- * Timetabling API
    retrieveMemberEvents,
    retrieveTermDates,
    retrieveTermDatesFor,
    retrieveTermWeeks,
    retrieveTermWeeksFor,
    retrieveHolidays
) where

--------------------------------------------------------------------------------

import Control.Monad.State
import Control.Monad.Except

import Data.Text.Encoding (encodeUtf8)
import qualified Data.HashMap.Lazy as HM
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Time

import Data.List (intercalate)

import Data.Aeson

import Network.HTTP.Conduit (newManager, tlsManagerSettings)

import Servant.API.BasicAuth
import Servant.Client

import Warwick.Config
import Warwick.Common hiding (TransportError)
import Warwick.Tabula.Config
import Warwick.Tabula.Types
import Warwick.Tabula.Coursework
import Warwick.Tabula.Member
import Warwick.Tabula.Payload
import Warwick.Tabula.MemberSearchFilter
import Warwick.Tabula.API
import qualified Warwick.Tabula.Internal as I
import Warwick.DownloadSubmission

-------------------------------------------------------------------------------

-- | 'withTabula' @instance config action@ runs the computation @action@
-- by connecting to @instance@ with the configuration specified by @config@.
withTabula :: TabulaInstance 
           -> APIConfig 
           -> Tabula a 
           -> IO (Either TabulaErr a)
withTabula inst APIConfig{..} m = do
    manager <- newManager tlsManagerSettings

    let auth = BasicAuthData
                    (encodeUtf8 apiUsername)
                    (encodeUtf8 apiPassword)
        url  = getBaseUrl inst
        env  = ClientEnv manager url
        sesh = APISession auth manager url

    r <- runClientM (runExceptT $ evalStateT m sesh) (env Nothing)

    case r of
        Left serr -> case serr of 
#if MIN_VERSION_servant_client(0,16,0)
            FailureResponse _ res -> case decode (responseBody res) of
#else 
            FailureResponse res -> case decode (responseBody res) of
#endif
                Nothing -> pure $ Left $ TransportError serr
                Just er -> pure $ Left er
            _ -> pure $ Left $ TransportError serr
        Right res -> pure res

-------------------------------------------------------------------------------

-- | Client functions generated by servant throw exceptions when a server
-- returns a non-2xx status code. 'handle' @m@ catches exceptions which are
-- thrown when @m@ is executed and tries to convert them into a Tabula response.
handle :: (FromJSON a, HasPayload a)
       => ClientM (TabulaResponse a) -> Tabula (TabulaResponse a)
handle m = lift $ lift m

-------------------------------------------------------------------------------

-- | `retrieveModule` @code@ retrieves the module identified by @code@.
retrieveModule :: ModuleCode -> Tabula (TabulaResponse Module)
retrieveModule mc = do 
    authData <- getAuthData
    handle $ I.retrieveModule authData mc

-- | 'listRegisteredUsercodes' @code@ retrieves the usercodes of all students
-- registered for @code@ in the current academic year.
listRegisteredUsercodes :: ModuleCode -> Tabula (TabulaResponse [Text])
listRegisteredUsercodes mc = do 
    authData <- getAuthData
    r <- handle $ I.listRegisteredUsercodes authData mc 
    pure $ fmap getUsercodes r

-- | 'listRegisteredUsercodesIn' @code academicYear@ retrieves the usercodes 
-- of all students registered for @code@ in the academic year given by
-- @academicYear@.
listRegisteredUsercodesIn :: ModuleCode -> Text -> Tabula (TabulaResponse [Text])
listRegisteredUsercodesIn mc year = do 
    authData <- getAuthData
    r <- handle $ I.listRegisteredUsercodesIn authData mc year
    pure $ fmap getUsercodes r

-- | 'listRegisteredUniversityIds' @code@ retrieves the University IDs of all
-- students registered for @code@ in the current academic year.
listRegisteredUniversityIds :: ModuleCode -> Tabula (TabulaResponse [Text])
listRegisteredUniversityIds mc = do 
    authData <- getAuthData
    r <- handle $ I.listRegisteredUniversityIds authData mc True
    pure $ fmap getUniversityIds r

-- | 'listRegisteredUniversityIdsIn' @code@ retrieves the University IDs of all
-- students registered for @code@ in the academic year given by
-- @academicYear@.
listRegisteredUniversityIdsIn :: ModuleCode -> Text -> Tabula (TabulaResponse [Text])
listRegisteredUniversityIdsIn mc year = do 
    authData <- getAuthData
    r <- handle $ I.listRegisteredUniversityIdsIn authData mc year True
    pure $ fmap getUniversityIds r

-- | 'listDepartments' retrieves information about all departments. 
listDepartments :: Tabula (TabulaResponse [Department])
listDepartments = do 
    authData <- getAuthData 
    handle $ I.listDepartments authData

-- | `retrieveDepartment` @deptCode@ retrieves information about the 
-- department identified by @deptCode@.
retrieveDepartment :: Text -> Tabula (TabulaResponse Department)
retrieveDepartment dept = do 
    authData <- getAuthData 
    handle $ I.retrieveDepartment authData dept

-- | 'listDepartmentModules' @deptCode@ retrieves a list of modules in the
-- department identified by @deptCode@.
listDepartmentModules :: Text -> Tabula (TabulaResponse [Module])
listDepartmentModules dept = do 
    authData <- getAuthData 
    handle $ I.listDepartmentModules authData dept

-------------------------------------------------------------------------------

listAssignments ::
    ModuleCode -> Maybe AcademicYear -> Tabula (TabulaResponse [Assignment])
listAssignments mc yr = do
    authData <- getAuthData
    handle $ I.listAssignments authData mc yr

retrieveAssignment ::
    ModuleCode -> AssignmentID -> [String] -> Tabula (TabulaResponse Assignment)
retrieveAssignment mc aid xs = do
    let fdata = if Prelude.null xs then Nothing else Just (intercalate "," xs)
    authData <- getAuthData
    handle $ I.retrieveAssignment authData mc (unAssignmentID aid) fdata

listSubmissions ::
    ModuleCode -> AssignmentID -> Tabula (TabulaResponse (HM.HashMap String (Maybe Submission)))
listSubmissions mc aid = do
    authData <- getAuthData
    handle $ I.listSubmissions authData mc (unAssignmentID aid)

-- | 'postMarks' @moduleCode assignmentId marks@ uploads the feedback 
-- contained in @marks@ for the assignment identified by @assignmentId@.
postMarks ::
    ModuleCode -> AssignmentID -> Marks -> Tabula (TabulaResponse None)
postMarks mc aid marks = do 
    authData <- getAuthData
    handle $ I.postMarks authData mc (unAssignmentID aid) marks

-------------------------------------------------------------------------------

-- | `listSmallGroupSets` @moduleCode maybeAcademicYear@ lists all the small
-- group sets for @moduleCode@. If @maybeAcademicYear@ is `Nothing`, the sets
-- for the current academic year are returned. Otherwise the sets for the 
-- specified academic year are returned.
listSmallGroupSets :: ModuleCode -> Maybe Text -> Tabula (TabulaResponse [SmallGroupSet])
listSmallGroupSets mc mYear = do 
    authData <- getAuthData 
    handle $ I.listSmallGroupSets authData mc mYear

-- | `retrieveSmallGroupAllocations` @moduleCode sgSetId@ lists the 
-- small group allocations for the small group set identified by @sgSetId@ for
-- the module identified by @moduleCode@.
retrieveSmallGroupAllocations :: ModuleCode -> Text -> Tabula (TabulaResponse SmallGroupAllocations)
retrieveSmallGroupAllocations mc setId = do 
    authData <- getAuthData 
    handle $ I.retrieveSmallGroupAllocations authData mc setId

-- | `retrieveSmallGroupAttendance` @smallGroupId@ retrieves attendance 
-- information for all events belonging to the small group identified by
-- @smallGroupId@.
retrieveSmallGroupAttendance :: 
    Text -> Tabula (TabulaResponse SmallGroupAttendanceResponse)
retrieveSmallGroupAttendance groupId = do 
    authData <- getAuthData 
    handle $ I.retrieveSmallGroupAttendance authData groupId

-------------------------------------------------------------------------------

retrieveMember :: String -> [String] -> Tabula (TabulaResponse Member)
retrieveMember uid fields = do
    let fdata = if Prelude.null fields then Nothing else Just (intercalate "," fields)
    authData <- getAuthData
    handle $ I.retrieveMember authData uid fdata

retrieveMembers :: [Text] 
                -> [Text] 
                -> Tabula (TabulaResponse (HM.HashMap Text Member))
retrieveMembers uids fields = do
    let fdata | Prelude.null fields = Nothing 
              | otherwise = Just (T.intercalate "," fields)
    authData <- getAuthData
    handle $ I.retrieveMembers authData uids fdata

-- | 'listRelationships' @universityId maybeRelationshipType@ retrieves all
-- relationships that the user identified by @universityId@ has with other
-- members, optionally filtering by @maybeRelationshipType@.
listRelationships :: Text 
                  -> Maybe Text 
                  -> Tabula (TabulaResponse [Relationship])
listRelationships uid mRelationshipType = do
    authData <- getAuthData
    handle $ I.listRelationships authData uid mRelationshipType

personAssignments ::
    String -> Maybe AcademicYear -> Tabula TabulaAssignmentResponse
personAssignments uid academicYear = do
    authData <- getAuthData
    lift $ lift $ I.personAssignments authData uid (pack <$> academicYear)

-- | `listMembers` @filterSettings offset limit@ 
listMembers ::
    MemberSearchFilter -> Int -> Int -> Tabula (TabulaResponse [Member])
listMembers MemberSearchFilter{..} offset limit = do 
    authData <- getAuthData
    handle $ I.listMembers 
        authData 
        filterDepartment
        filterAcademicYear
        (toSearchParam filterFields)
        (Just offset)
        (Just limit) 
        (toSearchParam $ map (pack . show) filterCourseTypes)
        (toSearchParam filterRoutes)
        (toSearchParam filterCourses)
        (toSearchParam filterModesOfAttendance)
        (toSearchParam $ map (pack . show) filterYearsOfStudy)
        (toSearchParam filterLevelCodes)
        (toSearchParam filterSprStatuses)
        (toSearchParam filterModules)
        (toSearchParam filterHallsOfResidence)  

-- | 'retrieveAttendance' @userId academicYear@ retrieves information about
-- the attendance of the user identified by @userId@, limited to
-- the academic year given by @academicYear@.
retrieveAttendance ::
    Text -> AcademicYear -> Tabula (TabulaResponse MemberAttendance)
retrieveAttendance user academicYear = do 
    authData <- getAuthData
    handle $ I.retrieveAttendance authData user (pack academicYear)

-- | 'listRelationshipTypes' retrieves information about all types of
-- relationships that Tabula is aware of.
listRelationshipTypes :: Tabula (TabulaResponse [RelationshipType])
listRelationshipTypes = getAuthData >>= handle . I.listRelationshipTypes

-- | 'listAgents' @department relationshipType@ retrieves a list of reduced
-- 'Member' values representing all members in @department@ who are agents
-- for a relationship of type @relationshipType@.
listAgents :: Text -> Text -> Tabula (TabulaResponse [Member])
listAgents department relationshipType = do 
    authData <- getAuthData
    r <- handle $ I.listAgents authData department relationshipType
    pure $ fmap getAgents r

-------------------------------------------------------------------------------

-- | 'retrieveMemberEvents' @universityID academicYear startDate endDate@
-- retrieves a list of 'EventOccurrence' objects for the user identified by
-- @universityID@. If specified, @academicYear@ restricts the academic year
-- for which events are retrieved. @startDate@ and @endDate@ can be used to
-- further narrow down the search range.
retrieveMemberEvents :: Text
                     -> Maybe Text 
                     -> Maybe Day 
                     -> Maybe Day 
                     -> Tabula (TabulaResponse [EventOccurrence])
retrieveMemberEvents user academicYear start end = do 
    authData <- getAuthData
    handle $ I.retrieveMemberEvents authData user academicYear start end

-- | `retrieveTermDates` retrieves information about an academic year's terms. 
-- By default, information for the current academic year is returned, but the 
-- academic year can be specified using the `retrieveTermDatesFor` function.
--
-- >>> retrieveTermDates 
-- Right (TabulaOK {tabulaStatus = "ok", tabulaData = [..]})
--
retrieveTermDates :: Tabula (TabulaResponse [Term])
retrieveTermDates = handle I.retrieveTermDates

-- | `retrieveTermDatesFor` @academicYear@ retrieves information about an 
-- academic year's terms. The academic year for which the term dates are
-- retrieved is specified by @academicYear@. This should be the four 
-- character year in which the academic year starts.
--
-- >>> retrieveTermDatesFor "2019"
-- Right (TabulaOK {tabulaStatus = "ok", tabulaData = [..]})
--
retrieveTermDatesFor :: Text -> Tabula (TabulaResponse [Term])
retrieveTermDatesFor academicYear =
    handle $ I.retrieveTermDatesFor academicYear

-- | `retrieveTermWeeks` @numberingSystem@ retrieves information
-- about the weeks in the current academic year. The week's names
-- are determined by the specified @numberingSystem@. If no value
-- is specified, the API defaults to `AcademicNumbering`.
--
-- >>> retrieveTermWeeksFor (Just TermNumbering)
-- Right (TabulaOK {tabulaStatus = "ok", tabulaData = [..]})
--
retrieveTermWeeks :: 
    Maybe NumberingSystem -> Tabula (TabulaResponse [Week])
retrieveTermWeeks numberingSystem =
    handle $ I.retrieveTermWeeks numberingSystem

-- | `retrieveTermWeeksFor` @academicYear numberingSystem@ retrieves 
-- information about the weeks in @academicYear@. The week's names
-- are determined by the specified @numberingSystem@. If no value
-- is specified, the API defaults to `AcademicNumbering`.
--
-- >>> retrieveTermWeeksFor "2019" (Just TermNumbering)
-- Right (TabulaOK {tabulaStatus = "ok", tabulaData = [..]})
--
retrieveTermWeeksFor :: 
    Text -> Maybe NumberingSystem -> Tabula (TabulaResponse [Week])
retrieveTermWeeksFor academicYear numberingSystem =
    handle $ I.retrieveTermWeeksFor academicYear numberingSystem

-- | `retrieveHolidays` retrieves information about holiday dates.
retrieveHolidays :: Tabula (TabulaResponse [Holiday])
retrieveHolidays = handle I.retrieveHolidays 

-------------------------------------------------------------------------------
