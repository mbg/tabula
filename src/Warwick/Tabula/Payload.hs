-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

-- | This module re-exports modules containing payload data types.
module Warwick.Tabula.Payload (
    module Payload
) where

--------------------------------------------------------------------------------

import Warwick.Tabula.Payload.Attendance as Payload
import Warwick.Tabula.Payload.CourseDetails as Payload
import Warwick.Tabula.Payload.Department as Payload
import Warwick.Tabula.Payload.EventOccurrence as Payload
import Warwick.Tabula.Payload.Holiday as Payload
import Warwick.Tabula.Payload.Marks as Payload
import Warwick.Tabula.Payload.Module as Payload
import Warwick.Tabula.Payload.MonitoringPoints as Payload
import Warwick.Tabula.Payload.None as Payload
import Warwick.Tabula.Payload.Note as Payload
import Warwick.Tabula.Payload.SmallGroup as Payload
import Warwick.Tabula.Payload.Term as Payload
import Warwick.Tabula.Payload.Week as Payload

--------------------------------------------------------------------------------
