module Handler.Autor (getAutorR
                    , getFindAutorR) where

import Import
import DbUtils (prefixRegex, systemError)
import Database.Persist.MongoDB ((=~.))
import qualified Data.Vector as V
import qualified Data.Text as T

getAutorR :: Int64 -> Handler Html
getAutorR lookupId = do
    (Entity _ autor) <- runDB $ getBy404 $ UniqueAutor lookupId
    --(Entity kopalniaId kopalnia) <- runDB $ getBy404 $ UniqueKopalnia 1
    --autor1 <- runDB $ insert $ Autor 1 (Just "Jerzy") "Szyłak"
    --autor2 <- runDB $ insert $ Autor 2 (Just "Michał") "Traczyk"
    --autor3 <- runDB $ insert $ Autor 3 (Just "Ewa") "Lipińska"
    --autor4 <- runDB $ insert $ Autor 4 (Just "Przemysław") "Mazur"
    --runDB $ insert $ KopalniaAutor autor1 kopalniaId AutorAut
    --runDB $ insert $ KopalniaAutor autor2 kopalniaId AutorRed
    --runDB $ insert $ KopalniaAutor autor3 kopalniaId AutorTlum
    --runDB $ insert $ KopalniaAutor autor4 kopalniaId AutorWyw
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
