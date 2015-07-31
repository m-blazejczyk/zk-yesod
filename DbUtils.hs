module DbUtils where

import Import
import qualified Data.Text as T
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Aeson (encode)
import Utils

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

-- This function is quite ugly at the moment, but at least it works.
-- TODO: It should be improved by using a monad combinator such as EitherT.
--       Example: http://stackoverflow.com/questions/13252889/elegant-haskell-case-error-handling-in-sequential-monads
-- The first argument is a validation/conversion function that takes the raw (but trimmed) value of type Text
-- and returns Either <error message> <converted value> in the Handler monad (to allow for database validation lookups).
-- If everything succeeds then this value will be passed to the second argument alongside the db lookup criterion,
-- typically applied against updateWhere.
processXEditable :: (Text -> Handler (Either Text a)) -> (Filter Kopalnia -> a -> Handler ()) -> Handler Text
processXEditable vald upd = do
    mLookupId <- lookupPostParam "pk"
    mLookupId2 <- return $ maybeRead mLookupId
    case mLookupId2 of
        Just lookupId -> do
            mValueRaw <- lookupPostParam "value"
            case mValueRaw of
                Just valueRaw -> do
                    eValue <- vald $ T.strip valueRaw
                    case eValue of
                        Right value -> do
                            let criterion = KopalniaLookupId ==. lookupId
                            cnt <- runDB $ count [criterion]
                            case cnt of
                                1 -> do
                                    upd criterion value
                                    sendResponseStatus status200 ("OK" :: Text)
                                _ -> sendResponseStatus badRequest400 ("Błąd systemu: nieistniejący identyfikator" :: Text)
                        Left err -> sendResponseStatus badRequest400 err
                _ -> sendResponseStatus badRequest400 ("Błąd systemu: brak zmiennej 'value'" :: Text)
        _ -> sendResponseStatus badRequest400 ("Błąd systemu: niepoprawna zmienna 'pk'" :: Text)
