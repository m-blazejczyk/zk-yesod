module Utils where

import Prelude
import Data.Maybe (listToMaybe)
import Data.Int (Int64)
import Data.Aeson (ToJSON, Value, object, (.=))
import qualified Data.Text as T
import Data.Vector (fromList)

data Result = Error T.Text | Success
    deriving (Show, Read)

combine :: T.Text -> [Result] -> Result
combine sep = foldl combine' Success
    where combine' (Error err) (Error acc) = Error $ T.concat [acc, sep, err]
          combine' (Error err) Success = Error err
          combine' Success (Error acc) = Error acc
          combine' Success Success = Success

-- This function converts a List of tuples to JSON.  This is how the generated JSON looks like:
-- {
--    topLevelName: [
--      { fstName: "first value from tuple", sndName: "second value from tuple" },
--      { fstName: "first value from tuple", sndName: "second value from tuple" },
--      ...
--    ]
-- }
tuplesToRawJson :: ToJSON e => T.Text -> T.Text -> T.Text -> [(e, T.Text)] -> Value
tuplesToRawJson topLevelName fstName sndName tuples = 
    let formatOne tpl = object [(fstName .= fst tpl), (sndName .= snd tpl)]
        formatAll = Prelude.map formatOne tuples
    in object [topLevelName .= fromList formatAll]

-- A safe form of read.  Returns an Int64 to make it compatible with lookup ids in the database.
-- Borrowed from http://hackage.haskell.org/package/txt-sushi-0.6.0/src/Database/TxtSushi/ParseUtil.hs
maybeRead :: Maybe T.Text -> Maybe Int64
maybeRead (Just txt) = (fmap fst . listToMaybe . reads . T.unpack) txt
maybeRead _ = Nothing

