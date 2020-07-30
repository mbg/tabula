-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.Tabula.Payload.Extension (
    ExtensionStatus(..),
    StudentAssignmentExtension(..)
) where 

-------------------------------------------------------------------------------

import Control.Monad

import Data.Aeson

import Warwick.Tabula.Types

-------------------------------------------------------------------------------

-- | Represents the status of an extension.
data ExtensionStatus
    = Unreviewed
    | Approved
    | Rejected
    | Revoked
    deriving (Eq, Show)

instance FromJSON ExtensionStatus where
    parseJSON (String "Unreviewed") = pure Unreviewed
    parseJSON (String "Approved")   = pure Approved
    parseJSON (String "Rejected")   = pure Rejected
    parseJSON (String "Revoked")    = pure Revoked
    parseJSON _                     = mzero

data StudentAssignmentExtension = StudentAssignmentExtension {
    studentAssignmentExtID        :: UUID,
    studentAssignmentExtState     :: ExtensionStatus,
    studentAssignmentExtReqExpiry :: Maybe DateTime,
    studentAssignmentExtExpiry    :: DateTime
} deriving (Eq, Show)

instance FromJSON StudentAssignmentExtension where
    parseJSON = withObject "StudentAssignmentExtension" $ \v ->
        StudentAssignmentExtension <$> v .: "id"
                                   <*> v .: "state"
                                   <*> v .:? "requestedExpiryDate"
                                   <*> v .: "expiryDate"


-------------------------------------------------------------------------------
