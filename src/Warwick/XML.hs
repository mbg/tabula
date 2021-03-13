--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.XML where 

--------------------------------------------------------------------------------

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (toStrict)
import Data.Char (toLower)
import Data.List.NonEmpty (fromList)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

import Network.HTTP.Media ((//))

import Servant.API

import Xeno.DOM

--------------------------------------------------------------------------------

-- | A type representing XML content.
data XML 

instance Accept XML where 
    contentTypes _ = fromList ["application" // "xml", "text" // "xml"]

--------------------------------------------------------------------------------

-- | `getAttribute` @node attributeName@ tries to look up the value of an 
-- attribute named @attributeName@ in the attributes of @node@. 
getAttribute :: Node -> ByteString -> Either String ByteString
getAttribute node name = case lookup name (attributes node) of 
    Nothing  -> Left $ "No such attribute: " ++ show name
    Just val -> pure val

class FromXML a where 
    parseXML :: Node -> Either String a

instance FromXML a => FromXML [a] where 
    parseXML node = mapM parseXML (children node)

--------------------------------------------------------------------------------

class FromByteString a where 
    fromByteString :: ByteString -> a

instance FromByteString Text where 
    fromByteString = decodeUtf8 

instance FromByteString Bool where 
    fromByteString bs = case map toLower (unpack bs) of 
        "true"  -> True 
        "false" -> False
        _ -> error "Not a boolean"

--------------------------------------------------------------------------------

instance FromXML a => MimeUnrender XML a where 
    mimeUnrender _ input = case parse (toStrict input) of 
        Left err -> Left (show err)
        Right node -> parseXML node 

--------------------------------------------------------------------------------
