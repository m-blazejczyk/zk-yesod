module DbUtils (
    getMaybe,
    getListM,
    wydawcyToJson,
    processXEditable
    ) where

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

-- This function processes an Ajax POST request coming from X-Editable.  Each such request should first be validated,
-- and then one or more database fields should be updated.
-- The first argument is a validation/conversion function that takes the raw (but trimmed) value of type Text
-- and returns Either <error message> <converted value> in the Handler monad (to allow for database validation lookups).
-- If everything succeeds then this value will be passed to a call to updateWhere with the result of the second argument
-- as the last parameter.
-- The first arguments in the argument functions are names of the POST variables.
-- If there is only one value in the POST request then this will be "value".
-- If there are more then they will be "value[var1]", "value[var2]", and so on.
-- The last argument is an array of expected POST parameter names, or an empty array if only a single value is expected.
-- TODO: This function should be improved by using a monad combinator such as EitherT.
--       Example: http://stackoverflow.com/questions/13252889/elegant-haskell-case-error-handling-in-sequential-monads
processXEditable :: (Text -> Text -> Handler (Either Text a))
                 -> (Text -> a -> [Update Kopalnia])
                 -> [Text]
                 -> Handler Text
processXEditable vald upd [] = processXEditable' vald upd ["value"]
processXEditable vald upd parNames = processXEditable' vald upd parNames

-- This function differs from the previous one in that it requires the list of parameter names not to be empty.
processXEditable' :: (Text -> Text -> Handler (Either Text a))
                     -> (Text -> a -> [Update Kopalnia])
                     -> [Text]
                     -> Handler Text
processXEditable' vald upd parNames = do
    mLookupId <- lookupPostParam "pk"
    mLookupId2 <- return $ maybeRead mLookupId
    case mLookupId2 of
        Just lookupId -> do
            results <- processParams vald upd parNames lookupId
            -- Call a function to process all 'Maybe' values
            result <- return $ combine "<br>" results
            case result of
                Success -> sendResponseStatus status200 ("OK" :: Text)
                Error err -> sendResponseStatus badRequest400 err
        _ -> sendResponseStatus badRequest400 ("Błąd systemu: niepoprawna wartość parametru 'pk'" :: Text)

-- Because the type of the db-bound value will be different for every parameter so we need processParams go all
-- the way and actually perform the db update as well.
-- The third parameter is the Kopalnia lookup id.
-- The result if a list of Maybe values: for Just, they contain error messages; for Nothing, they indicate success.
processParams :: (Text -> Text -> Handler (Either Text a))
              -> (Text -> a -> [Update Kopalnia])
              -> [Text]
              -> Int64
              -> Handler [Result]
processParams vald upd parNames lookupId = mapM process1Param parNames
    where 
        process1Param parName = do
            mValueRaw <- lookupPostParam parName
            case mValueRaw of
                Just valueRaw -> do
                    eValue <- vald parName (T.strip valueRaw)
                    case eValue of
                        Right value -> do
                            let criterion = KopalniaLookupId ==. lookupId
                            cnt <- runDB $ count [criterion]
                            case cnt of
                                1 -> do
                                    runDB $ updateWhere [criterion] (upd parName value)
                                    return Success
                                _ -> return $ Error $ T.concat ["Błąd systemu: fiszka o tym identyfikatorze nie istnieje: ", (pack $ show lookupId)]
                        Left err -> return $ Error err
                _ -> return $ Error $ T.concat ["Błąd systemu: brak parametru ", parName]
