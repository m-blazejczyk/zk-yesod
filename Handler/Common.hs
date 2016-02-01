module Handler.Common (getFaviconR,
                       getRobotsR,
                       processSearchQuery) where

import Data.FileEmbed (embedFile)
import Import
import DbUtils (prefixRegex)
import Utils (systemError)
import Data.Vector as V
import Database.Persist.MongoDB (MongoRegex)

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.
getFaviconR :: Handler TypedContent
getFaviconR = return $ TypedContent "image/x-icon"
                     $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")

-- Expected format:
-- {
--   "more": false,
--   "results": [
--     {id: 1, text: 'Michał Błażejczyk'},
--     {id: 2, text: 'Jerzy Szyłak'},
--     {id: 3, text: 'Wojciech Birek'}
--   ]
-- }
processSearchQuery :: (PersistEntity ent, YesodPersist site,
                       PersistQuery (PersistEntityBackend ent),
                       PersistEntityBackend ent ~ YesodPersistBackend site) =>
                      (MongoRegex -> [Filter ent]) -> (Entity ent -> Value) -> HandlerT site IO Value
processSearchQuery crit transform = do
    mQ <- lookupGetParam "q"
    case mQ of
        Just q -> do
            let regex = prefixRegex q
            aut <- runDB $ selectList (crit regex) []
            let autJson = fmap transform aut
            returnJson $ object ["more" .= False, "results" .= V.fromList autJson]
        _ -> sendResponseStatus badRequest400 (systemError "Brak zapytania")
