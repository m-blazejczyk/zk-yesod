module Utils where

import Prelude
import Data.Maybe (listToMaybe)
import Data.Int (Int64)
import Data.Aeson (ToJSON, Value, object, (.=))
import Data.Text
import Data.Vector (fromList)
--import Text.Read (reads)

-- This function converts a List of tuples to JSON.  This is how the generated JSON looks like:
-- {
--    topLevelName: [
--      { fstName: "first value from tuple", sndName: "second value from tuple" },
--      { fstName: "first value from tuple", sndName: "second value from tuple" },
--      ...
--    ]
-- }
tuplesToRawJson :: ToJSON e => Text -> Text -> Text -> [(e, Text)] -> Value
tuplesToRawJson topLevelName fstName sndName tuples = 
    let formatOne tpl = object [(fstName .= fst tpl), (sndName .= snd tpl)]
        formatAll = Prelude.map formatOne tuples
    in object [topLevelName .= fromList formatAll]

-- A safe form of read.  Returns an Int64 to make it compatible with lookup ids in the database.
-- Borrowed from http://hackage.haskell.org/package/txt-sushi-0.6.0/src/Database/TxtSushi/ParseUtil.hs
maybeRead :: Maybe Text -> Maybe Int64
maybeRead (Just txt) = (fmap fst . listToMaybe . reads . unpack) txt
maybeRead _ = Nothing

