--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.MyWarwick.Response where 

--------------------------------------------------------------------------------

import Data.Aeson
import Data.Text
import Data.UUID.Types

--------------------------------------------------------------------------------

-- | Represents errors returned by the MyWarwick API.
data MyWarwickError = MyWarwickError {
    -- | The ID (e.g. @no-permission@) of the error.
    mweID :: Text, 
    -- | The message of the error.
    mweMessage :: Text
}

instance FromJSON MyWarwickError where 
    parseJSON = withObject "MyWarwickError" $ \obj ->
        MyWarwickError <$> obj .: "id"
                       <*> obj .: "message"

-- | Represents responses to requests that create stream items.
data StreamID = StreamID {
    -- | The UUID of the stream item that was created.
    streamID :: UUID
}

instance FromJSON StreamID where 
    parseJSON = withObject "StreamID" $ \obj ->
        StreamID <$> obj .: "id"

-- | Represents responses from the MyWarwick API.
data MyWarwickResponse = MyWarwickResponse {
    -- | A value indicating whether the request was successful.
    mwrSuccess :: Bool,
    -- | A status message.
    mwrStatus :: Text,
    -- | If the request was successful, this contains any data that
    -- was returned.
    mwrData :: Maybe StreamID,
    -- | If the request failed, this contains a list of errors.
    mwrErrors :: Maybe [MyWarwickError]
}

instance FromJSON MyWarwickResponse where
    parseJSON = withObject "MyWarwickResponse" $ \obj ->
        MyWarwickResponse <$> obj .: "success"
                          <*> obj .: "status"
                          <*> obj .: "data"
                          <*> obj .: "errors"

--------------------------------------------------------------------------------
