-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.Tabula.Payload.WeekRange (
    WeekRange(..)
) where 

-------------------------------------------------------------------------------

import Data.Aeson 

-------------------------------------------------------------------------------

-- | Represents an object specifying the week number range for a term.
data WeekRange = WeekRange {
    -- | The number of the first week in the term.
    weekRangeMin :: Int,
    -- | The number of the last week in the term.
    weekRangeMax :: Int
} deriving (Eq, Show)

instance ToJSON WeekRange where 
    toJSON WeekRange{..} =
        object [ "minWeek" .= weekRangeMin 
               , "maxWeek" .= weekRangeMax 
               ]

instance FromJSON WeekRange where 
    parseJSON = withObject "WeekRange" $ \obj ->
        WeekRange <$> obj .: "minWeek"
                  <*> obj .: "maxWeek"

-------------------------------------------------------------------------------
