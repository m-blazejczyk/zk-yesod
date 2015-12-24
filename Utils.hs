module Utils where

import Prelude
import Data.Maybe (listToMaybe)
import Data.Int (Int64)
import Data.Aeson (ToJSON, Value, object, (.=))
import qualified Data.Text as T
import Data.Vector (fromList)

data Result a = Error T.Text | Success a
    deriving (Show, Read)

-- Copied from source code of Control.Monad.Either.
instance Functor Result where
    fmap _ (Error a) = Error a
    fmap f (Success a) = Success (f a)
instance Monad Result where
    return = Success
    Success m >>= k = k m
    Error e >>= _ = Error e
instance Applicative Result where
  pure = Success
  a <*> b = do x <- a; y <- b; return (x y)

-- This function turns an array of results into a single result that will have:
--  * Either the array of results (with order preserved) as value if all results were successes,
--  * Or a concatenated (using 'sep') error message if there was at least one error.
combine :: T.Text -> [Result a] -> Result [a]
combine sep arr = rev $ foldl combine' (Success []) arr
    where combine' (Error acc) (Error err) = Error $ T.concat [acc, sep, err]
          combine' (Success _) (Error err) = Error err
          combine' (Error acc) (Success _) = Error acc
          combine' (Success acc) (Success val) = Success (val:acc)
          rev (Success val) = Success $ reverse val
          rev (Error err) = Error err

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

-- Empty text gets converted to Nothing; non-empty one to Just Text.
textToMaybe :: T.Text -> Maybe T.Text
textToMaybe txt
    | T.length txt > 0 = Just txt
    | otherwise = Nothing

-- Access to elements of a 3-element tuple.
fst3 :: (a, b, c) -> a
fst3 (val, _, _) = val

snd3 :: (a, b, c) -> b
snd3 (_, val, _) = val

trd3 :: (a, b, c) -> c
trd3 (_, _, val) = val

-- Prepends 'Błąd systemu: ' to the given message.
systemError :: T.Text -> T.Text
systemError msg = T.concat ["Błąd systemu: ", msg]

-- Prepends 'Błąd systemu: ' to the given message, and then appends a colon and the second argument.
systemErrorT :: T.Text -> T.Text -> T.Text
systemErrorT msg argT = T.concat ["Błąd systemu: ", msg, ": ", argT]

-- Prepends 'Błąd systemu: ' to the given message, and then appends a colon and the second argument.
-- ('show'ed and 'pack'ed).
systemErrorS :: Show a => T.Text -> a -> T.Text
systemErrorS msg argS = T.concat ["Błąd systemu: ", msg, ": ", (T.pack $ show argS)]

-- Given a list of map keys, and the map itself, return a list of values.
lookupParams :: [T.Text] -> [(T.Text, T.Text)] -> [Maybe T.Text]
lookupParams names vals = map (\name -> lookup name vals) names
