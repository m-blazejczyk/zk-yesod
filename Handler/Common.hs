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
