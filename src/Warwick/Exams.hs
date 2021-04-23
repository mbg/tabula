-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.Exams (
    module Warwick.Common,
    module Warwick.Exams.ExamProfile,
    Exams,
    getExamProfiles
) where

-------------------------------------------------------------------------------

import Data.Proxy

import Servant.API
import Servant.Client

import Warwick.ApiInstance
import Warwick.Common
import Warwick.Response
import Warwick.Exams.ExamProfile

-------------------------------------------------------------------------------

data Exams

instance ServiceName Exams where
    serviceName = "exams"

-------------------------------------------------------------------------------

type ExamsAPI
    = "api" :>
      "v1" :>
      "examProfiles.json" :>
      Get '[JSON] (StandardResponse [ExamProfile])

-- | A proxy value for the `ExamsAPI` type.
exams :: Proxy ExamsAPI
exams = Proxy

-- | `getExamProfiles` retrieves the list of exam profiles.
getExamProfiles :: ClientM (StandardResponse [ExamProfile])
getExamProfiles = client exams

-------------------------------------------------------------------------------
