module Handler.Autor (getAutorR) where

import Import
import Enums

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
