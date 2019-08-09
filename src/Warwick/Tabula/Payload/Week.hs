--------------------------------------------------------------------------------
-- Haskell bindings for the Tabula API                                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Tabula.Payload.Week where 

--------------------------------------------------------------------------------

import Data.Aeson
import Data.Time.Clock
import Data.Text (Text)

import Servant.API

import Warwick.Tabula.Types

--------------------------------------------------------------------------------

-- | Enumerates different methods for numbering weeks.
data NumberingSystem
    -- | Weeks run from 1 to 52 throughout the year
    = AcademicNumbering
    -- | The first week of the first term is Term 1, Week 1; 
    -- the first week of the second term is Term 2, Week 1
    | TermNumbering
    -- The first week of the first term is Week 1; 
    -- the first week of the second term is Week 11 
    | CumulativeNumbering 
    -- Use dates.
    | NoNumbering

instance ToHttpApiData NumberingSystem where
    toQueryParam AcademicNumbering   = "academic"
    toQueryParam TermNumbering       = "term"
    toQueryParam CumulativeNumbering = "cumulative"
    toQueryParam NoNumbering         = "none"

-- | Represents a week in an academic year.
data Week = Week {
    -- | The academic week number.
    weekNumber :: Int,
    -- | The first day of the week.
    weekStart :: Date,
    -- | The last day of the week.
    weekEnd :: Date,
    -- | The name of the week, based on some `NumberingSystem`.
    weekName :: Text
} deriving Show

instance FromJSON Week where 
    parseJSON = withObject "Week" $ \obj ->
        Week <$> obj .: "weekNumber"
             <*> obj .: "start"
             <*> obj .: "end"
             <*> obj .: "name"

instance HasPayload [Week] where
    payloadFieldName _ = "weeks"

--------------------------------------------------------------------------------
