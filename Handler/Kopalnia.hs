module Handler.Kopalnia (getKopalniaMainR, getKopalniaItemR, getKopalniaRodzic) where

import Import
-- import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, withSmallInput)

getKopalniaMainR :: Handler Html
getKopalniaMainR = do
    -- autor1 <- runDB $ insert $ Autor (Just "Piotr") "Marczewski"
    -- autor2 <- runDB $ insert $ Autor (Just "Anna") "Kowalczyk"
    -- autorzy <- M.fromList [(AutorAut, autor1), (AutorTlum, autor2)]
    -- now <- liftIO $ getCurrentTime
    -- nk1 <- runDB $ insert $ NkPub "Polityka"
    -- klucz1 <- runDB $ insert $ SlowoKlucz "Klucz 3"
    -- klucz2 <- runDB $ insert $ SlowoKlucz "Klucz 4"
    -- haslo1 <- runDB $ insert $ HasloPrzedm "Grafika"
    -- haslo2 <- runDB $ insert $ HasloPrzedm "Muzyka"
    -- autor1 <- runDB $ insert $ Autor (Just "Jerzy") "Szyłak"
    -- autor2 <- runDB $ insert $ Autor (Just "Michał") "Traczyk"
    -- wydawca <- runDB $ insert $ Wydawca "Timof i cisi wspólnicy"
    -- item1 <- runDB $ insert $ Kopalnia Nothing Nothing Nothing (Just "Rodzic") (Just "Strony") (Just "ISBN") "Tytul" autorzy Nothing Nothing [] [] (Just 2014) (Just 12) False (Just wydawca) (Just "Poznań") Numer (Just "8 str.") JezykPL now now
    defaultLayout $ do
        setTitle "Polska Bibliografia Wiedzy o Komiksie - Zeszyty Komiksowe"
        $(widgetFile "kopalnia-main")

getMaybe :: (PersistEntity val, PersistStore (YesodPersistBackend site),
             YesodPersist site,
             PersistEntityBackend val ~ YesodPersistBackend site) =>
            Maybe (Key val) -> HandlerT site IO (Maybe val)
getMaybe (Just lookupId) = runDB $ get lookupId
getMaybe _ = return Nothing

getKopalniaItemR :: Int64 -> Handler Html
getKopalniaItemR lookupId = do
    (Entity _ kopalnia) <- runDB $ getBy404 $ UniqueKopalnia lookupId
    mRodzicEn <- getMaybe $ kopalniaRodzicId kopalnia
    mNkRodzicEn <- case mRodzicEn of
        Just _ -> return Nothing
        Nothing -> getMaybe $ kopalniaNkRodzicId kopalnia
    mDzialEn <- case mRodzicEn of
        Just _ -> return Nothing
        Nothing -> getMaybe $ kopalniaDzialId kopalnia
    defaultLayout $ do
        setTitle "Fiszka publikacji - Polska Bibliografia Wiedzy o Komiksie - Zeszyty Komiksowe"
        $(widgetFile "kopalnia-item")

-- Format:
-- <span class="inobtrusive">W:</span> RodzicId.Tytul, RodzicOpis <span class="inobtrusive">(RodzicId.Rodzaj; dział:</span> DzialId->Tytul <span class="inobtrusive">)</span>
-- Options: 
-- W: RodzicId, RodzicOpis (RodzajRodzica; dział: DzialId)
-- W: RodzicId, RodzicOpis (RodzajRodzica)
-- W: RodzicId (RodzajRodzica)
-- W: RodzicId (RodzajRodzica; dział: DzialId)
-- W: RodzicOpis
getKopalniaRodzic :: Kopalnia -> Text
getKopalniaRodzic _ = "<em>hello!</em>"