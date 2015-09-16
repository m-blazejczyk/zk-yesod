{-# LANGUAGE ExtendedDefaultRules #-}  -- Required by MongoDB stuff

module DbUtils (
    getMaybe,
    getListM,
    prefixRegex,
    wydawcyToJson
    ) where

import Import
import qualified Data.Text as T
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Aeson (encode)
import Utils
import Database.MongoDB ((=:))
import Database.Persist.MongoDB (MongoRegex)
import qualified Database.MongoDB as MongoDB

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

-- Sample MongoDB query for authors:
--   db.Autor.find({ $or: [{ imiona: { $regex: '^to', $options: 'i' } }, { nazwisko: { $regex: '^ma', $options: 'i' } }] })
-- MongoDB docs: http://hackage.haskell.org/package/mongoDB-2.0.6/docs/Database-MongoDB.html
-- Custom MongoDB quesies in Yesod: https://github.com/yesodweb/yesod/wiki/Raw-Mongo
-- I'm keeping these two functions here as reference.
autorzyQuery :: [MongoDB.Field]
autorzyQuery = [ "$or" =: [ [ "imiona" =: [ MongoDB.Regex "^to" "i" ] ]
                          , [ "nazwisko" =: [ MongoDB.Regex "^ma" "i" ] ] ] ]

autorzyJsonTxt :: (MonadBaseControl IO m, MonadIO m) => MongoDB.Action m Text
autorzyJsonTxt = do
    docs <- MongoDB.rest =<< MongoDB.find (MongoDB.select autorzyQuery "Autor")
    let arr = MongoDB.Array $ fmap MongoDB.Doc docs
    return $ T.pack $ show arr

-- Returns all publishers as JSON, in the following format suitable for the select2 control:
-- {
--   "source": [
--     { "text": "Timof i cisi wspólnicy", "id": 1 },
--     ...
--   ]
-- }
wydawcyToJson :: Handler Data.ByteString.Lazy.Internal.ByteString
wydawcyToJson = do
    wydawcyDb <- runDB $ selectList [] [Asc WydawcaNazwa]  -- returns [(Key val, val)]
    wydawcy <- mapM (\(Entity _ wyd) -> return (wydawcaLookupId wyd, wydawcaNazwa wyd)) wydawcyDb  -- now we have [(Int64, Text)]
    return $ encode $ tuplesToRawJson "source" "id" "text" wydawcy
