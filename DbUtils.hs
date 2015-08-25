module DbUtils (
    getMaybe,
    getListM,
    wydawcyToJson,
    processXEditable,
    processXEditable1
    ) where

import Import
import qualified Data.Text as T
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Aeson (encode)
import Utils
import Database.MongoDB ((=:))
import Database.MongoDB (Document, Action, findOne)
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

rawOne :: MonadIO m => MongoDB.Collection -> MongoDB.Selector -> Action m (Maybe Document)
rawOne collection q = findOne (MongoDB.select q collection)

theCollection :: MonadIO m => MongoDB.Selector -> Action m (Maybe Document)
theCollection = rawOne $ "collection-name"

getTheR :: MongoDB.Val v => v -> Handler (Maybe Document)
getTheR theId = runDB $ theCollection ["_id" =: theId]

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
--   and then one or more database fields should be updated.
-- The first argument is a validation/conversion function that takes a list of the raw (but trimmed) values of type Text
--   and returns Result <converted value> in the Handler monad (to allow for database validation lookups).
--   The value array is ordered in the same way as the third argument to processXEditable.
--   The point of passing an entire array is to allow for validation across parameters.
-- If everything succeeds then the second argument function (upd) will be called to store the values returned by vald
--   in the database.
-- The last argument is an array of expected POST parameter names, or an empty array if only a single value is expected.
-- TODO: This function should be improved by using a monad combinator such as EitherT.
--       Example: http://stackoverflow.com/questions/13252889/elegant-haskell-case-error-handling-in-sequential-monads
processXEditable :: ([Text] -> Handler (Result a))
                 -> (Filter Kopalnia -> a -> Handler (Result Text))
                 -> [Text]
                 -> Handler Text
processXEditable vald upd [] = processXEditable' vald upd [""]
processXEditable vald upd parNames = processXEditable' vald upd parNames

-- This is a version of processXEditable for POST requests requiring a single parameter value.
-- The upd function in this case should simply return a value that will be passed to a call to runDb $ updateWhere.
processXEditable1 :: (Text -> Handler (Result a))
                  -> (a -> [Update Kopalnia])
                  -> Handler Text
processXEditable1 vald' upd' = processXEditable' vald upd [""]
    where
        vald params = case headMay params of
            Just h -> vald' h
            Nothing -> return $ Error "Błąd systemu: brakuje parametrów"
        upd criterion vals = do
            runDB $ updateWhere [criterion] (upd' vals)
            return $ Success "OK"

-- This function differs from the previous one in that it requires the list of parameter names not to be empty.
processXEditable' :: ([Text] -> Handler (Result a))
                  -> (Filter Kopalnia -> a -> Handler (Result Text))
                  -> [Text]
                  -> Handler Text
processXEditable' vald upd parNames = do
    mLookupId <- lookupPostParam "pk"
    mLookupId2 <- return $ maybeRead mLookupId
    case mLookupId2 of
        Just lookupId -> do
            -- Make sure that all value parameters exist, and turn them into an array, or combine errors.
            lParams <- getNamedParams parNames
            rParams <- return $ combine "\n" lParams
            case rParams of
                Success params -> do
                    -- Run the user-provided cross-validation function.
                    parsOk <- vald params
                    case parsOk of
                        Success vals -> do
                            let criterion = KopalniaLookupId ==. lookupId
                            cnt <- runDB $ count [criterion]
                            case cnt of
                                1 -> do
                                    -- Run the user-provided database update function.
                                    res <- upd criterion vals
                                    case res of
                                        Error err -> sendResponseStatus badRequest400 err
                                        Success msg -> sendResponseStatus status200 msg
                                _ -> sendResponseStatus badRequest400 (T.concat ["Błąd systemu: fiszka o tym identyfikatorze nie istnieje: ", (pack $ show lookupId)])
                        Error err -> sendResponseStatus badRequest400 err
                Error err -> sendResponseStatus badRequest400 err
        _ -> sendResponseStatus badRequest400 ("Błąd systemu: niepoprawna wartość albo brak parametru pk" :: Text)

getNamedParams :: [Text] -> Handler [Result Text]
getNamedParams = mapM get1Param
    where 
        get1Param parName = do
            actualName <- return $ getActualName parName
            mValueRaw <- lookupPostParam actualName
            case mValueRaw of
                Just valueRaw -> return $ Success $ T.strip valueRaw
                Nothing -> return $ Error $ T.concat ["Błąd systemu: brak parametru ", parName]
        getActualName "" = "value"
        getActualName parName = T.concat ["value[", parName, "]"]
