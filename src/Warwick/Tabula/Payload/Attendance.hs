--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Tabula.Payload.Attendance (
    MemberAttendance(..)
) where 

--------------------------------------------------------------------------------

import Data.Aeson
import Data.Aeson.Types

import Warwick.Tabula.Types
import Warwick.Tabula.Payload.MonitoringPoints
import Warwick.Tabula.Payload.SmallGroup

--------------------------------------------------------------------------------

data MemberAttendance = MemberAttendance {
    maMonitoringPoints :: Maybe [MonitoringPointAttendance],
    maSmallGroups :: [SmallGroupSetAttendance]
} deriving (Eq, Show)

fromJSONObject :: Object -> Parser MemberAttendance
fromJSONObject obj = 
    MemberAttendance <$> obj .:? "monitoringPoints"
                     <*> obj .: "smallGroups"

instance FromJSON MemberAttendance where 
    parseJSON = withObject "MemberAttendance" fromJSONObject

instance HasPayload MemberAttendance where 
    payloadFieldName _ = 
        error "payloadFieldName should not be used for MemberAttendance"

    payload = fromJSONObject

--------------------------------------------------------------------------------