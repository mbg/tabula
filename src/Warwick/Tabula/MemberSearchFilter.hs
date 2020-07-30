--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Tabula.MemberSearchFilter where 

--------------------------------------------------------------------------------

import Data.Text as T

import Warwick.Tabula.Types

--------------------------------------------------------------------------------

data CourseType
    = UG 
    | PGT 
    | PGR
    | Foundation 
    | PreSessional
    deriving Show 

data MemberSearchFilter = MemberSearchFilter {
    -- | The department codes to filter by.
    filterDepartment :: [Text],
    -- | The academic year to filter by.
    filterAcademicYear :: Maybe AcademicYear,
    -- | The fields to retrieve for all results.
    filterFields :: [Text],
    -- | The course types to filter by.
    filterCourseTypes :: [CourseType],
    -- | The routes to filter by.
    filterRoutes :: [Text],
    -- | The courses to filter by.
    filterCourses :: [Text],
    -- | The modes of attendance to filter by.
    filterModesOfAttendance :: [Text],
    -- | The years of study to filter by.
    filterYearsOfStudy :: [Int],
    -- | The level codes to filter by.
    filterLevelCodes :: [Text],
    filterSprStatuses :: [Text],
    -- | The modules to filter by.
    filterModules :: [Text],
    -- | The halls of residence to filter by.
    filterHallsOfResidence :: [Text]
}

toSearchParam :: [Text] -> Maybe Text 
toSearchParam [] = Nothing 
toSearchParam xs = Just (T.intercalate "," xs)

-- | `defaultMemberSearch` represents an empty search filter.
defaultMemberSearch :: MemberSearchFilter
defaultMemberSearch = MemberSearchFilter {
    filterDepartment = [],
    filterAcademicYear = Nothing,
    filterFields = [],
    filterCourseTypes = [],
    filterRoutes = [],
    filterCourses = [],
    filterModesOfAttendance = [],
    filterYearsOfStudy = [],
    filterLevelCodes = [],
    filterSprStatuses = [],
    filterModules = [],
    filterHallsOfResidence = []
}

--------------------------------------------------------------------------------
