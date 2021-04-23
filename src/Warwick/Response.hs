-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

-- | Many University of Warwick APIs have a common response format. This
-- module implements a type to represent that common response format which
-- is shared among different services.
module Warwick.Response (
    HasPayload(..),
    StandardResponse(..)
) where

-------------------------------------------------------------------------------

import Data.Aeson
import Data.Aeson.Types
import Data.Text

-------------------------------------------------------------------------------

class FromJSON a => HasPayload a where
    -- | `payloadFieldName` represents the payload field name.
    payloadFieldName :: Text

    -- | `payload` @object@ parses the payload given a JSON @object@. The
    -- payload is assumed to be associated with a key identified by
    -- `payloadFieldName`.
    payload :: Object -> Parser a
    payload v = v .: payloadFieldName @a

-------------------------------------------------------------------------------

-- | Represents a standard API response.
data StandardResponse a = MkStandardResponse {
    -- | The response's payload.
    responsePayload :: a
} deriving (Eq, Show)

instance (HasPayload a, FromJSON a) => FromJSON (StandardResponse a) where
    parseJSON = withObject "StandardResponse" $ \v -> do
        s <- v .: "success"
        if s
        then MkStandardResponse <$> payload v
        else fail "200 OK, but success is not true (this should not happen)"

-------------------------------------------------------------------------------
