{-# LANGUAGE ExtendedDefaultRules #-}  -- Required by MongoDB stuff

module DbUtils (
    getMaybe,
    getListM,
    prefixRegex,
    wydawcyToJson,
    autorzyToFieldValue
    ) where

import Import
import qualified Data.Text as T
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Aeson (encode)
import Utils
import Database.Persist.MongoDB (MongoRegex)

-- Turns a (Maybe <database id>) to a (Maybe <entity>).
getMaybe :: (PersistEntity ent, PersistStore (YesodPersistBackend site),
             YesodPersist site,
             PersistEntityBackend ent ~ YesodPersistBackend site) =>
            Maybe (Key ent) -> HandlerT site IO (Maybe ent)
getMaybe (Just lookupId) = runDB $ get lookupId
getMaybe _ = return Nothing

-- Retrieves the entities referred to by the given list of database keys, or Nothing.
getListM :: (PersistEntity ent, PersistStore (YesodPersistBackend site),
             YesodPersist site,
             PersistEntityBackend ent ~ YesodPersistBackend site) =>
            [Key ent] -> HandlerT site IO [(Maybe ent)]
getListM = mapM (\key -> runDB $ get key)

-- Helper function to create a Regex search term for MongoDB.
prefixRegex :: Text -> MongoRegex -- i.e. (Text, Text)
prefixRegex q = (,) (T.concat ["^", q]) "i"

-- Returns all publishers as JSON, in the following format suitable for the select2 control:
-- {
--   "source": [
--     { "text": "Timof i cisi wspólnicy", "value": 1 },
--     ...
--   ]
-- }
wydawcyToJson :: Handler Data.ByteString.Lazy.Internal.ByteString
wydawcyToJson = do
    wydawcyDb <- runDB $ selectList [] [Asc WydawcaNazwa]  -- returns [(Key val, val)]
    wydawcy <- mapM (\(Entity _ wyd) -> return (wydawcaLookupId wyd, wydawcaNazwa wyd)) wydawcyDb  -- now we have [(Int64, Text)]
    return $ encode $ tuplesToRawJson "source" "value" "text" wydawcy

autorzyToFieldValue :: [Autor] -> T.Text
autorzyToFieldValue _ = ""
