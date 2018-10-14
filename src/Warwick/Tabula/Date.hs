
module Warwick.Tabula.Date where

import Data.Aeson
import qualified Data.Text as T
import Data.Time
import Data.Time.ISO8601

newtype TabulaDateTime = TabulaDateTime { getDateTime :: UTCTime }
    deriving Show

instance FromJSON TabulaDateTime where
    parseJSON = withText "ISO8601 date+time format" $
        \str -> case parseISO8601 (T.unpack str) of
            Nothing -> fail "Date and time not formatted in ISO8601."
            Just d  -> return (TabulaDateTime d)

newtype TabulaDate = TabulaDate { getDate :: UTCTime }
    deriving Show

instance FromJSON TabulaDate where
    parseJSON = withText "ISO8601 date format" $
        \str -> case parseTimeM True defaultTimeLocale "%F" (T.unpack str) of
            Nothing -> fail "Date not formatted in ISO8601."
            Just d  -> return (TabulaDate d)
