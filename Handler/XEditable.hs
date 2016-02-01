module Handler.XEditable (
    XEdVal(..),
    EditHandler,
    lookupEditParam,
    lookupEditHandler,
    processXEditable,
    processXEditable1,
    processXEditableMulti,
    valdMap,
    valdArr
    ) where

import Import
import Database.MongoDB.Query (MongoContext)
import qualified Data.List as L
import qualified Data.Text as T
import Utils (Result(..), maybeRead, systemError, systemErrorT, systemErrorS, lookupParams)

-- Type representing a function that takes a map of POST parameters as argument.
type EditHandler = [(Text, Text)] -> Handler Text

-- Given the field identifier (of type 'a') returns the expected POST parameter name.
-- If the lookup failed, returns 'error'.
lookupEditParam :: Eq a => a -> [(a, (Text, EditHandler))] -> Text
lookupEditParam field handlers =
    case L.lookup field handlers of
        Just pair -> fst pair
        Nothing -> "error"

-- Given the name of the POST parameter containing the field name, returns the handler.
-- First looks up 'lookupName' in 'params' and then looks up the resulting text value
-- in (fst.snd) of 'handlers'.
lookupEditHandler :: Text -> [(Text, Text)] -> [(a, (Text, EditHandler))] -> Maybe EditHandler
lookupEditHandler lookupName params handlers = do
    field <- L.lookup lookupName params
    pair <- L.find (\item -> ((fst . snd) item) == field) handlers
    return $ (snd . snd) pair

data XEdVal = XEdNone                       -- No "value*" parameter was found at all
            | XEdValOne T.Text              -- A "value" parameter was found
            | XEdValArr [T.Text]            -- Several "value[]" parameters were found
            | XEdValMap [(T.Text, T.Text)]  -- Several "value[*]" parameters were found
    deriving (Show)

-- This is a version of processXEditable for POST requests requiring a single parameter value.
-- The upd function in this case should simply return a value that will be passed to a call
--   to runDb $ updateWhere.
processXEditable1 :: (Text -> Handler (Result a))
                  -> (a -> [Update Kopalnia])
                  -> [(T.Text, T.Text)]
                  -> Handler Text
processXEditable1 vald upd = processXEditable vald' upd'
    where
        vald' (XEdValOne val) = vald val
        vald' _ = return $ Error $ systemError "Brakuje parametru"
        upd' lookupId vals = do
            runDB $ updateWhere [KopalniaLookupId ==. lookupId] (upd vals)
            return $ Success "OK"

-- This function processes an Ajax POST request coming from X-Editable that contains a select2
-- field for a many-to-many relationship with Kopalnia with Int64 identifiers as values.
--
-- Important: when sending the request that contains no ids (or, an empty list of ids),
-- there should be a simple "value[]" parameter with the value of "".
--
-- 'rec' is the entity linked to Kopalnia via intermediate table 'm2m'.
--
-- getUnique is usually just the constructor of the 'Unique' datatype for 'rec'.
-- extractId is a simple extractor of values of type 'Key rec' from 'Entity rec', wrapped in a Maybe.
-- delFilter is used to delete records from 'm2m' for a given Key Kopalnia.
-- insRecord is used to insert new records into 'm2m' using two keys.
-- processNewItem is used to insert new records into 'rec' (for tags).
-- tableName is the name of the 'rec' table in the database.
processXEditableMulti :: (PersistEntity rec, PersistEntity m2m,
                          PersistEntityBackend rec ~ Database.MongoDB.Query.MongoContext,
                          PersistEntityBackend m2m ~ Database.MongoDB.Query.MongoContext)
                       => (Int64 -> Unique rec)
                       -> (Maybe (Entity rec) -> Maybe (Key rec))
                       -> (Key Kopalnia -> [Filter m2m])
                       -> (Key rec -> Key Kopalnia -> m2m)
                       -> (Text -> Handler (Key rec))
                       -> Text
                       -> [(Text, Text)]
                       -> Handler Text
processXEditableMulti getUnique extractId delFilter insRecord processNewItem tableName = processXEditable (valdArr vald) upd
    where
        -- Handling of the ("value[]","") parameter (no ids, i.e. empty set).
        vald [""] = return $ Success $ []
        vald arr = 
            -- mapMaybe :: (Maybe Text -> Maybe Int64) -> [Maybe Text] -> [Int64]
            -- mapMaybe filters out all Nothing values from the list so if any id was invalid
            -- then the result will be shorter.
            let arrIds = mapMaybe maybeRead (map Just arr)
                newItems = filter (isPrefixOf "NEW:") arr
            in if length arrIds + length newItems == length arr
                then valdDb arrIds (map (drop 4) newItems)
                else return $ Error $ systemErrorT "Niepoprawny identyfikator tabeli" tableName
        valdDb lookupIds newItems = do
            -- mapM :: (a -> Handler (Maybe (Entity x y))) -> [a] -> Handler [Maybe (Entity x y)]
            mRecords <- mapM (\iden -> runDB $ getBy $ getUnique iden) lookupIds  -- mRecords :: [Maybe (Entity x y)]
            let ids = mapMaybe extractId mRecords  -- :: [Key rec]
            newIds <- mapM processNewItem newItems -- :: [Key rec]
            if length ids == length lookupIds
                then return $ Success $ (L.nub (ids ++ newIds))
                else return $ Error $ systemErrorT "Niezdefiniowany identyfikator tabeli" tableName
        -- 'recordIds' is of type [Key Autor]
        upd kopalniaLookupId recordIds = do
            mKopalnia <- runDB $ getBy $ UniqueKopalnia kopalniaLookupId
            case mKopalnia of
                Just (Entity kopalniaId _) -> do
                    runDB $ deleteWhere $ delFilter kopalniaId
                    _ <- mapM (\recordId -> runDB $ insert $ insRecord recordId kopalniaId) recordIds
                    return $ Success "OK"
                -- This should NEVER happen!
                Nothing -> return $ Success $ systemErrorS "Fiszka o tym identyfikatorze nie istnieje" kopalniaLookupId

-- This function processes an Ajax POST request coming from X-Editable.  Each such request should
--   first be validated, and then one or more database fields should be updated.
-- The first argument is the map of POST parameters.
-- The second argument is a validation/conversion function that takes an XEdVal and returns
--   Result <converted value> in the Handler monad (to allow for database validation lookups).
-- If everything succeeds then the third argument function (upd) will be called to store the values
--   returned by vald in the database.
processXEditable :: (XEdVal -> Handler (Result a))
                 -> (Int64 -> a -> Handler (Result Text))
                 -> [(T.Text, T.Text)]
                 -> Handler Text
processXEditable vald upd params = do
    let mLookupIdT = lookup "pk" params
    let mLookupIdI = maybeRead mLookupIdT
    case mLookupIdI of
        Just lookupId -> do
            -- Get the value representing all "value*" POST parameters.
            let xEdVals = getXEdValues params
            case xEdVals of
                XEdNone -> sendResponseStatus badRequest400 (systemError "Brak parametru 'value'")
                _ -> do
                    -- Run the user-provided cross-validation function.
                    parsOk <- vald xEdVals
                    case parsOk of
                        Success vals -> do
                            let criterion = KopalniaLookupId ==. lookupId
                            cnt <- runDB $ count [criterion]
                            case cnt of
                                1 -> do
                                    -- Run the user-provided database update function.
                                    res <- upd lookupId vals
                                    case res of
                                        Error err -> sendResponseStatus badRequest400 err
                                        Success msg -> sendResponseStatus status200 msg
                                _ -> sendResponseStatus badRequest400 (systemErrorS "Fiszka o tym identyfikatorze nie istnieje" lookupId)
                        Error err -> sendResponseStatus badRequest400 err
        _ -> sendResponseStatus badRequest400 (systemError "Niepoprawny format albo brak parametru 'pk'")

-- This function takes a map of POST parameters and produces an XEdVal.
-- In a sense, it performs the auto-discovery of the type of XEditable value,
-- i.e. is it a single value, an array, or a map (many named values).
getXEdValues :: [(T.Text, T.Text)] -> XEdVal
getXEdValues = foldr process XEdNone
    where
        -- All of these are for cases when any parameter named 'value*' hasn't been seen yet.
        process ("value", pv) XEdNone = XEdValOne pv
        process ("value[]", pv) XEdNone = XEdValArr [pv]
        process (pn, pv) XEdNone | isMap pn = XEdValMap $ mkMap pn pv []
                                 | otherwise = XEdNone
        -- If we already have a XEdValOne then ignore all further parameters.
        process _ vOne@(XEdValOne _) = vOne
        -- If we have a XEdValArr and found another "value[]" then we append; otherwise - ignore.
        process ("value[]", pv) (XEdValArr oldArr) = XEdValArr (pv:oldArr)
        process _ vArr@(XEdValArr _) = vArr
        -- If we have a XEdValMap and found another "value[xxx]" then we append; otherwise - ignore.
        process (pn, pv) vMap@(XEdValMap oldMap) | isMap pn = XEdValMap $ mkMap pn pv oldMap
                                                 | otherwise = vMap
        -- Helper functions.
        mkMap pn pv oldMap = ((T.drop 6 . T.dropEnd 1) pn, pv):oldMap
        isMap pn = T.length pn > 7 && T.isPrefixOf "value[" pn && T.isSuffixOf "]" pn

-- This function simplifies the construction of the validation function for processXEditable
-- when the expected POST values are a map.
-- The first argument is an array of expected parameter names, and the second one is a function
-- that takes an array (in the same order as the first array) of parameter values and validates
-- them.
valdMap :: [T.Text] -> ([Maybe T.Text] -> Handler (Result a)) -> (XEdVal -> Handler (Result a))
valdMap names vald = wrapper
    where 
        wrapper (XEdValMap vMap) = vald $ lookupParams names vMap
        wrapper _ = return $ Error $ systemError "Niepoprawna forma parametrów"

-- This function simplifies the construction of the validation function for processXEditable
-- when the expected POST values are an array.
-- The argument is a function that takes an array of parameter values and validates them.
valdArr :: ([T.Text] -> Handler (Result a)) -> (XEdVal -> Handler (Result a))
valdArr vald = wrapper
    where
        wrapper (XEdValArr vArr) = vald vArr
        wrapper _ = return $ Error $ systemError "Niepoprawna forma parametrów"
