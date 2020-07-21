-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.Tabula.Payload.Term (
    Term(..)
) where 

--------------------------------------------------------------------------------

import Data.Aeson
import Data.Text

import Warwick.Tabula.Types
import Warwick.Tabula.Payload.WeekRange

--------------------------------------------------------------------------------

-- | Represents information about an academic term.
data Term = Term {
    -- | The name of the term.
    termName :: Text,
    -- | The first day of the term.
    termStart :: Date,
    -- | The last day of the term.
    termEnd :: Date,
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
