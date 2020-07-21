-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.Tabula.Payload.EventOccurrence (
    EventOccurrence(..)
) where 

-------------------------------------------------------------------------------

import Data.Aeson
import Data.Text
import Data.Time

import Warwick.Tabula.Types
import Warwick.Tabula.Member
import Warwick.Tabula.Payload.Location

-------------------------------------------------------------------------------

-- | https://warwick.ac.uk/services/its/servicessupport/web/tabula/api/timetabling/event-occurrence-object
data EventOccurrence = EventOccurrence {
    -- | A unique identifier for the timetable event. For Syllabus+ events, 
    -- this is generated from an MD5 hex of the name, start time, end time, 
    -- day of week, location name and context.
    occurrenceID :: Text,
    -- | The name of the timetable event, e.g. CS141L.
    occurrenceName :: Text,
    -- | The human-readable of the timetable event. Typically empty for 
    -- Syallabus+ events.
    occurrenceTitle :: Text,
    -- | A description of the timetable event.
    occurrenceDescription :: Text,
    -- | The type of event.
    occurrenceType :: Text,
    -- | The start time of the event.
    occurrenceStart :: LocalTime,
    -- | The end time of the event.
    occurrenceEnd :: LocalTime,
    -- | An object representing the location of the event, or 'Nothing' if 
    -- there isn't one. 
    occurrenceLocation :: Maybe Location,
    -- | The context of the timetable event. This is typically the module
    -- code, e.g. CS141
    occurrenceContext :: Maybe Text,
    -- | Any comments for the timetable event, or 'Nothing' if none exist.
    occurrenceComments :: Maybe Text,
    -- | A list of University IDs for staff members who are linked to 
    -- delivering this event.
    occurrenceStaffUniversityIDs :: [Text],
    -- | A list of 'Member' objects, with the following properties:
    -- - firstName
    -- - lastName
    -- - email
    -- - userType
    occurrenceStaff :: [Member]
} deriving (Eq, Show)

instance FromJSON EventOccurrence where 
    parseJSON = withObject "EventOccurrence" $ \obj -> 
        EventOccurrence <$> obj .: "uid"
                        <*> obj .: "name"
                        <*> obj .: "title"
                        <*> obj .: "description"
                        <*> obj .: "eventType"
                        <*> obj .: "start"
                        <*> obj .: "end"
                        <*> obj .:? "location"
                        <*> obj .:? "context"
                        <*> obj .:? "comments"
                        <*> obj .: "staffUniversityIds"
                        <*> obj .: "staff"

instance HasPayload [EventOccurrence] where 
    payloadFieldName _ = "events"

-------------------------------------------------------------------------------
