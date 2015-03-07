module Handler.Kopalnia where

import Import
import Enums
-- import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, withSmallInput)

getKopalniaMainR :: Handler Html
getKopalniaMainR = do
    now <- liftIO $ getCurrentTime
    nk1 <- runDB $ insert $ NkPub "Polityka"
    klucz1 <- runDB $ insert $ SlowoKlucz "Klucz 3"
    klucz2 <- runDB $ insert $ SlowoKlucz "Klucz 4"
    haslo1 <- runDB $ insert $ HasloPrzedm "Grafika"
    haslo2 <- runDB $ insert $ HasloPrzedm "Muzyka"
    -- autor1 <- runDB $ insert $ Autor (Just "Jerzy") "Szyłak"
    -- autor2 <- runDB $ insert $ Autor (Just "Michał") "Traczyk"
    wydawca <- runDB $ insert $ Wydawca "Timof i cisi wspólnicy"
    item1 <- runDB $ insert $ Kopalnia Nothing (Just nk1) Nothing (Just "Rodzic") (Just "Strony") (Just "ISBN") "Tytul" Nothing Nothing [klucz1, klucz2] [haslo1, haslo2] (Just 2014) (Just 12) False (Just wydawca) (Just "Poznań") Numer (Just "8 str.") JezykPL now now
    defaultLayout $ do
        setTitle "Polska Bibliografia Wiedzy o Komiksie - Zeszyty Komiksowe"
        $(widgetFile "kopalnia-main")
