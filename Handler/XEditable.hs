module Handler.XEditable (
    processXEditable,
    processXEditable1
    ) where

import Import
import qualified Data.Text as T
import Utils (Result(..), maybeRead, combine, systemError, systemErrorT, systemErrorS)
-- import DbUtils

data XEdVal = XEdNone | XEdValOne T.Text | XEdValArr [T.Text] | XEdValMap [(T.Text, T.Text)]

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
            Nothing -> return $ Error $ systemError "Brakuje parametrów"
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
                                _ -> sendResponseStatus badRequest400 (systemErrorS "Fiszka o tym identyfikatorze nie istnieje" lookupId)
                        Error err -> sendResponseStatus badRequest400 err
                Error err -> sendResponseStatus badRequest400 err
        _ -> sendResponseStatus badRequest400 (systemError "Niepoprawna wartość albo brak parametru pk")

-- This function should return a value of type HttpVal that has 3 constructors: for a single value,
-- for an array and for a map.  The function will auto-detect the constructor to use.  Then, validation
-- functions will look at these values and return errors if the value is of a wrong type.  It could even
-- happen in wrapper functions in this here module.  Also, passing around the array of parameter names
-- won't be necessary - this function should call lookupPostParams.
getNamedParams :: [Text] -> Handler [Result Text]
getNamedParams = mapM get1Param
    where 
        get1Param parName = do
            actualName <- return $ getActualName parName
            mValueRaw <- lookupPostParam actualName
            case mValueRaw of
                Just valueRaw -> return $ Success $ T.strip valueRaw
                Nothing -> return $ Error $ systemErrorT "Brak parametru" parName
        getActualName "" = "value"
        getActualName parName = T.concat ["value[", parName, "]"]

getXEdValues :: Handler XEdVal
getXEdValues = do
    (pp, _) <- runRequestBody
    return $ foldr process XEdNone pp
    where
        process ("value", pv) XEdNone = XEdValOne pv
        process ("value[]", pv) XEdNone = XEdValArr [pv]
        process (pn, pv) XEdNone | T.isPrefixOf "value[" pn && T.isSuffixOf "]" pn = XEdValMap [(T.drop 6 pn, pv)]
                                 | otherwise = XEdNone
        process _ vOne@(XEdValOne v) = vOne
        process (pn, pv) (XEdValArr vArr) = undefined
        process (pn, pv) (XEdValMap vMap) = undefined
