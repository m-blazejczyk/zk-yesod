{-# LANGUAGE ExtendedDefaultRules #-}  -- Required by MongoDB stuff

module DbUtils (
    getMaybe,
    getListM,
    getListMany2ManyEx,
    getListMany2Many,
    prefixRegex,
    wydawcyToJson,
    itemsToFieldValue,
    wydawcyToFieldValue
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

getListMany2ManyEx :: (PersistEntity ki, PersistEntity i, YesodPersist site,
                       PersistStore (YesodPersistBackend site),
                       PersistQuery (PersistEntityBackend ki),
                       YesodPersistBackend site ~ PersistEntityBackend ki,
                       YesodPersistBackend site ~ PersistEntityBackend i) =>
                      [Filter ki] -> (ki -> Key i) -> HandlerT site IO ([Entity ki], [Maybe i])
getListMany2ManyEx crit accessor = do
    -- This call return a List of [Entity KopalniaX]
    kopItems <- runDB $ selectList crit []
    -- This call returns a List of Maybe X
    mItems <- mapM (\(Entity _ k) -> runDB $ get $ accessor k) kopItems
    return $ (kopItems, mItems)

getListMany2Many :: (PersistEntity ki, PersistEntity i, YesodPersist site,
                     PersistStore (YesodPersistBackend site),
                     PersistQuery (PersistEntityBackend ki),
                     YesodPersistBackend site ~ PersistEntityBackend ki,
                     YesodPersistBackend site ~ PersistEntityBackend i) =>
                    [Filter ki] -> (ki -> Key i) -> HandlerT site IO [i]
getListMany2Many crit accessor = do
    (_, mItems) <- getListMany2ManyEx crit accessor
    return $ catMaybes mItems

-- Helper function to create a Regex search term for MongoDB.
prefixRegex :: Text -> MongoRegex -- i.e. (Text, Text)
prefixRegex q = (,) (T.concat ["^", q]) "i"

-- Returns all publishers as JSON, in the following format suitable for the select2 control:
-- {
--   "data": [
--     { "text": "Timof i cisi wspólnicy", "id": 1 },
--     ...
--   ]
-- }
wydawcyToJson :: Handler Data.ByteString.Lazy.Internal.ByteString
wydawcyToJson = do
    wydawcyDb <- runDB $ selectList [] [Asc WydawcaNazwa]  -- returns [(Key val, val)]
    wydawcy <- mapM (\(Entity _ wyd) -> return (wydawcaLookupId wyd, wydawcaNazwa wyd)) wydawcyDb  -- now we have [(Int64, Text)]
    return $ encode $ tuplesToRawJson "data" "id" "text" wydawcy

itemsToFieldValue :: (a -> Int64) -> (a -> T.Text) -> [a] -> T.Text
itemsToFieldValue lookupFun nameFun items = T.intercalate "||" (map processItem items)
    where processItem item = T.intercalate "||" [(T.pack . show) (lookupFun item), nameFun item]

wydawcyToFieldValue :: [Wydawca] -> T.Text
wydawcyToFieldValue = itemsToFieldValue wydawcaLookupId wydawcaNazwa
