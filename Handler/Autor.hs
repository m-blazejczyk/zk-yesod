module Handler.Autor (getAutorR,
                      getFindAutorR
                      ) where

import Import
import DbUtils (prefixRegex)
import Utils (systemError)
import Database.Persist.MongoDB ((=~.))
import qualified Data.Vector as V
import qualified Data.Text as T

getAutorR :: Int64 -> Handler Html
getAutorR lookupId = do
    (Entity _ autor) <- runDB $ getBy404 $ UniqueAutor lookupId
    defaultLayout $ do
        setTitle "Fiszka autora - Polska Bibliografia Wiedzy o Komiksie - Zeszyty Komiksowe"
        $(widgetFile "autor")

-- Expected format:
-- {
--   "more": false,
--   "results": [
--     {id: 1, text: 'Michał Błażejczyk'},
--     {id: 2, text: 'Jerzy Szyłak'},
--     {id: 3, text: 'Wojciech Birek'}
--   ]
-- }
getFindAutorR :: Handler Value
getFindAutorR = do
    mQ <- lookupGetParam "q"
    case mQ of
        Just q -> do
            let regex = prefixRegex q
            aut <- runDB $ selectList ([AutorImiona =~. regex] ||. [AutorNazwisko =~. regex]) []
            let autJson = fmap transform aut
            returnJson $ object ["more" .= False, "results" .= V.fromList autJson]
        _ -> sendResponseStatus badRequest400 (systemError "Brak zapytania")
    where
        transform (Entity _ autor) = object ["id" .= autorLookupId autor, "text" .= getName autor]
        getName autor = T.concat [getFName autor, autorNazwisko autor]
        getFName autor = maybe "" (\i -> T.concat [i, " "]) (autorImiona autor)
