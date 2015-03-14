module Handler.Kopalnia (getKopalniaMainR, getKopalniaItemR) where

import Import
import Enums
-- import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, withSmallInput)

getKopalniaMainR :: Handler Html
getKopalniaMainR = do
    --autor1 <- runDB $ insert $ Autor (Just "Piotr") "Marczewski"
    --autor2 <- runDB $ insert $ Autor (Just "Anna") "Kowalczyk"
    --autorzy <- M.fromList [(AutorAut, autor1), (AutorTlum, autor2)]
    --now <- liftIO $ getCurrentTime
    --nk1 <- runDB $ insert $ NkPub 1 "Polityka" Pismo
    --klucz1 <- runDB $ insert $ SlowoKlucz "Klucz 1"
    --klucz2 <- runDB $ insert $ SlowoKlucz "Klucz 2"
    --haslo1 <- runDB $ insert $ HasloPrzedm "Grafika"
    --haslo2 <- runDB $ insert $ HasloPrzedm "Muzyka"
    --autor1 <- runDB $ insert $ Autor (Just "Jerzy") "Szyłak"
    --autor2 <- runDB $ insert $ Autor (Just "Michał") "Traczyk"
    --wydawca <- runDB $ insert $ Wydawca "Timof i cisi wspólnicy"
    --nastepny <- runDB $ insert $ Nastepny 2 2
    --item1 <- runDB $ insert $ Kopalnia 1 Nothing (Just nk1) Nothing (Just "Opis rodzica") (Just "Strony") (Just "ISBN") "Tytul" (Just "Nota") (Just "Opis") [klucz1, klucz2] [haslo1, haslo2] (Just 2014) (Just 12) False (Just wydawca) (Just "Poznań") Numer (Just "8 str.") JezykPL now now
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
    mRodzic <- getMaybe $ kopalniaRodzicId kopalnia
    mNkRodzic <- case mRodzic of
        Just _ -> return Nothing
        Nothing -> getMaybe $ kopalniaNkRodzicId kopalnia
    mDzial <- case mRodzic of
        Just _ -> getMaybe $ kopalniaDzialId kopalnia
        Nothing -> return Nothing
    defaultLayout $ do
        setTitle "Fiszka publikacji - Polska Bibliografia Wiedzy o Komiksie - Zeszyty Komiksowe"
        $(widgetFile "kopalnia-item")

-- Format:
-- <span class="inobtrusive">W:</span> RodzicId.Tytul, RodzicOpis <span class="inobtrusive">(RodzicId.Rodzaj; dział:</span> DzialId->Tytul <span class="inobtrusive">)</span>
-- Options (RodzicId can be a Kopalnia or a NkPub; NkPubs don't have Dzial): 
-- W: RodzicId, RodzicOpis (RodzajRodzica; dział: DzialId)
-- W: RodzicId, RodzicOpis (RodzajRodzica)
-- W: RodzicId (RodzajRodzica; dział: DzialId)
-- W: RodzicId (RodzajRodzica)
-- W: RodzicOpis
--
-- This approach is not DRY at all, but at the same time it simplifies the conditional HTML building a lot.
-- getRodzicW rodzicOpis rodzic dzial nkRodzic
getRodzicW :: Maybe Text -> Maybe Kopalnia -> Maybe Kopalnia -> Maybe NkPub -> Widget
getRodzicW (Just opis) (Just rodzic) (Just dzial) Nothing =
    toWidget [hamlet|
    <p>
        <span class="inobtrusive">W:
        <a href=@{KopalniaItemR (kopalniaLookupId rodzic)} class="link1">#{kopalniaTytul rodzic}#
        \, #{opis}
        <span class="inobtrusive">(#{showRodzaj $ kopalniaRodzaj rodzic}; dział:
        <a href=@{KopalniaItemR (kopalniaLookupId dzial)} class="link1">#{kopalniaTytul dzial}
        <span class="inobtrusive">)
    |]
getRodzicW Nothing     (Just rodzic) (Just dzial) Nothing = 
    toWidget [hamlet|
    <p>
        <span class="inobtrusive">W:
        <a href=@{KopalniaItemR (kopalniaLookupId rodzic)} class="link1">#{kopalniaTytul rodzic}
        <span class="inobtrusive">(#{showRodzaj $ kopalniaRodzaj rodzic}; dział:
        <a href=@{KopalniaItemR (kopalniaLookupId dzial)} class="link1">#{kopalniaTytul dzial}
        <span class="inobtrusive">)
    |]
getRodzicW (Just opis) (Just rodzic) Nothing      Nothing = 
    toWidget [hamlet|
    <p>
        <span class="inobtrusive">W:
        <a href=@{KopalniaItemR (kopalniaLookupId rodzic)} class="link1">#{kopalniaTytul rodzic}#
        \, #{opis}
        <span class="inobtrusive">(#{showRodzaj $ kopalniaRodzaj rodzic})
    |]
getRodzicW Nothing     (Just rodzic) Nothing      Nothing = 
    toWidget [hamlet|
    <p>
        <span class="inobtrusive">W:
        <a href=@{KopalniaItemR (kopalniaLookupId rodzic)} class="link1">#{kopalniaTytul rodzic}
        <span class="inobtrusive">(#{showRodzaj $ kopalniaRodzaj rodzic})
    |]
getRodzicW (Just opis) Nothing       Nothing      (Just nkRodzic) = 
    toWidget [hamlet|
    <p>
        <span class="inobtrusive">W:
        <a href="#" class="link1">#{nkPubTytul nkRodzic}#
        \, #{opis}
        <span class="inobtrusive">(#{showRodzaj $ nkPubRodzaj nkRodzic})
    |]
getRodzicW Nothing     Nothing       Nothing      (Just nkRodzic) = 
    toWidget [hamlet|
    <p>
        <span class="inobtrusive">W:
        <a href="#" class="link1">#{nkPubTytul nkRodzic}
        <span class="inobtrusive">(#{showRodzaj $ nkPubRodzaj nkRodzic})
    |]
getRodzicW (Just opis) Nothing       Nothing      Nothing = 
    toWidget [hamlet|
    <p>
        <span class="inobtrusive">W:
        #{opis}
    |]
getRodzicW Nothing     Nothing       Nothing      Nothing = 
    return ()
getRodzicW _ _ _ _ = 
    return ()  -- This is an error condition
