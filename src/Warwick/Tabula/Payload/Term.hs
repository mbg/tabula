--------------------------------------------------------------------------------
-- Haskell bindings for the Tabula API                                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Tabula.Payload.Term where 

--------------------------------------------------------------------------------

import Data.Aeson
import Data.Text

import Warwick.Tabula.Types

--------------------------------------------------------------------------------

-- | Represents an object specifying the week number range for a term.
data WeekRange = WeekRange {
    -- | The number of the first week in the term.
    weekRangeMin :: Int,
    -- | The number of the last week in the term.
    weekRangeMax :: Int
} deriving Show

instance ToJSON WeekRange where 
    toJSON WeekRange{..} =
        object [ "minWeek" .= weekRangeMin 
               , "maxWeek" .= weekRangeMax 
               ]

instance FromJSON WeekRange where 
    parseJSON = withObject "WeekRange" $ \obj ->
        WeekRange <$> obj .: "minWeek"
                  <*> obj .: "maxWeek"

-- | Represents information about an academic term.
data Term = Term {
    -- | The name of the term.
    termName :: Text,
    -- | The first day of the term.
    termStart :: TabulaDate,
    -- | The last day of the term.
    termEnd :: TabulaDate,
    -- | The week numbers for the term.
    termWeekRange :: WeekRange
} deriving Show

instance ToJSON Term where 
    toJSON Term{..} = 
        object [ "name"      .= termName 
               , "start"     .= termStart 
               , "end"       .= termEnd 
               , "weekRange" .= termWeekRange 
               ]

instance FromJSON Term where 
    parseJSON = withObject "Term" $ \obj ->
        Term <$> obj .: "name"
             <*> obj .: "start"
             <*> obj .: "end"
             <*> obj .: "weekRange"

instance HasPayload [Term] where 
    payloadFieldName _ = "terms"

--------------------------------------------------------------------------------
