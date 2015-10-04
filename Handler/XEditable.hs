module Handler.XEditable (
    XEdVal(..),
    processXEditable,
    processXEditable1,
    valdMap
    ) where

import Import
import qualified Data.Text as T
import Utils (Result(..), maybeRead, systemError, systemErrorS, lookupParams)

data XEdVal = XEdNone                       -- No "value*" parameter was found at all
            | XEdValOne T.Text              -- A "value" parameter was found
            | XEdValArr [T.Text]            -- Several "value[]" parameters were found
            | XEdValMap [(T.Text, T.Text)]  -- Several "value[*]" parameters were found
    deriving (Show)

-- This is a version of processXEditable for POST requests requiring a single parameter value.
-- The upd function in this case should simply return a value that will be passed to a call to runDb $ updateWhere.
processXEditable1 :: (Text -> Handler (Result a))
                  -> (a -> [Update Kopalnia])
                  -> Handler Text
processXEditable1 vald upd = processXEditable vald' upd'
    where
        vald' (XEdValOne val) = vald val
        vald' _ = return $ Error $ systemError "Brakuje parametru"
        upd' criterion vals = do
            runDB $ updateWhere [criterion] (upd vals)
            return $ Success "OK"

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
processXEditable :: (XEdVal -> Handler (Result a))
                 -> (Filter Kopalnia -> a -> Handler (Result Text))
                 -> Handler Text
processXEditable vald upd = do
    (params, _) <- runRequestBody
    let mLookupIdT = lookup "pk" params
    let mLookupIdI = maybeRead mLookupIdT
    case mLookupIdI of
        Just lookupId -> do
            -- Get the value representing all "value*" POST parameters.
            let xEdVals = getXEdValues params
            case xEdVals of
                XEdNone -> sendResponseStatus badRequest400 (systemError "Brak parametru value")
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
                                    res <- upd criterion vals
                                    case res of
                                        Error err -> sendResponseStatus badRequest400 err
                                        Success msg -> sendResponseStatus status200 msg
                                _ -> sendResponseStatus badRequest400 (systemErrorS "Fiszka o tym identyfikatorze nie istnieje" lookupId)
                        Error err -> sendResponseStatus badRequest400 err
        _ -> sendResponseStatus badRequest400 (systemError "Niepoprawny format albo brak parametru pk")

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
