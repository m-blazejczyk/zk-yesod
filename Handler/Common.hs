-- | Common handler functions.
module Handler.Common where

import Data.FileEmbed (embedFile)
import qualified Data.List as L
import Import

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.
getFaviconR :: Handler TypedContent
getFaviconR = return $ TypedContent "image/x-icon"
                     $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")

-- Type representing a function that takes a map of POST parameters as argument.
type EditHandler = [(Text, Text)] -> Handler Text

getEditParamName :: Eq a => a -> [(a, (Text, EditHandler))] -> Maybe Text
getEditParamName field dat = do
    pair <- L.lookup field dat
    return $ fst pair

getEditHandler :: Text -> [(a, (Text, EditHandler))] -> Maybe EditHandler
getEditHandler name dat = do
    pair <- L.find (\item -> ((fst . snd) item) == name) dat
    return $ (snd . snd) pair

lookupHandler :: Text -> [(Text, Text)] -> [(a, (Text, EditHandler))] -> Maybe EditHandler
lookupHandler lookupName params handlers = do
    field <- L.lookup lookupName params
    pair <- L.find (\item -> ((fst . snd) item) == field) handlers
    return $ (snd . snd) pair
