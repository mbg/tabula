--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Tabula.MemberSearchFilter where 

--------------------------------------------------------------------------------

import Data.Text as T

--------------------------------------------------------------------------------

data CourseType
    = UG 
    | PGT 
    | PGR
    | Foundation 
    | PreSessional
    deriving Show 

data MemberSearchFilter = MemberSearchFilter {
    filterDepartment :: [Text],
    filterFields :: [Text],
    filterCourseTypes :: [CourseType],
    filterRoutes :: [Text],
    filterCourses :: [Text],
    filterModesOfAttendance :: [Text],
    filterYearsOfStudy :: [Int],
    filterLevelCodes :: [Text],
    filterSprStatuses :: [Text],
    filterModules :: [Text],
    filterHallsOfResidence :: [Text]
}

toSearchParam :: [Text] -> Maybe Text 
toSearchParam [] = Nothing 
toSearchParam xs = Just (T.intercalate "," xs)

-- | `defaultMemberSearch` represents an empty search filter.
defaultMemberSearch :: MemberSearchFilter
defaultMemberSearch = MemberSearchFilter {
    filterDepartment = [],
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
