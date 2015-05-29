module Handler.Kopalnia (getKopalniaMainR, getKopalniaItemR, getKopalniaItemEditR) where

import Import
import Enums
-- import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, withSmallInput)

defaultTitle :: Html
defaultTitle = "Polska Bibliografia Wiedzy o Komiksie - Zeszyty Komiksowe"

getKopalniaMainR :: Handler Html
getKopalniaMainR = do
    -- autor1 <- runDB $ insert $ Autor 1 (Just "Piotr") "Marczewski"
    -- autor2 <- runDB $ insert $ Autor 2 (Just "Jerzy") "Szyłak"
    -- autor3 <- runDB $ insert $ Autor 3 (Just "Michał") "Traczyk"
    -- autor4 <- runDB $ insert $ Autor 4 (Just "Tomasz") "Marciniak"
    -- now <- liftIO $ getCurrentTime
    -- nk1 <- runDB $ insert $ NkPub 1 "Polityka" Pismo
    -- klucz1 <- runDB $ insert $ SlowoKlucz "Klucz 1"
    -- klucz2 <- runDB $ insert $ SlowoKlucz "Klucz 2"
    -- haslo1 <- runDB $ insert $ HasloPrzedm "Grafika"
    -- haslo2 <- runDB $ insert $ HasloPrzedm "Muzyka"
    -- wydawca <- runDB $ insert $ Wydawca 1 "Timof i cisi wspólnicy" (Just "http://www.timof.pl")
    -- link1 <- runDB $ insert $ KopalniaLink "http://www.zeszytykomiksowe.org" Nothing Nothing
    -- link2 <- runDB $ insert $ KopalniaLink "http://www.google.com" (Just "Google") Nothing
    -- link3 <- runDB $ insert $ KopalniaLink "http://www.pgx.ca" Nothing (Just "Strona PGx")
    -- item1 <- runDB $ insert $ Kopalnia 1 Nothing Nothing (Just nk1) Nothing Nothing Nothing Nothing "Komiks i jego konteksty" (Just "nk") Nothing (Just "Olo") (Just "kk") Nothing (Just "To jest opis") (Just 1999) Nothing True Nothing Nothing Artykul Nothing JezykPL [] [] [] now now
    -- item2 <- runDB $ insert $ Kopalnia 2 Nothing Nothing Nothing Nothing (Just "Rodzic") Nothing (Just "1111-111") "Zeszyty Komiksowe" Nothing Nothing Nothing Nothing Nothing (Just "To jest opis") (Just 2007) (Just 10) False (Just wydawca) (Just "Poznań") Pismo (Just "128 str.") JezykPL [klucz1, klucz2] [haslo1, haslo2] [link1] now now
    -- item3 <- runDB $ insert $ Kopalnia 3 (Just "http://www.zeszytykomiksowe.org") (Just item2) Nothing (Just item1) Nothing (Just "22-27") (Just "2222-111") "Dlaczego nie lubię komiksów" Nothing Nothing Nothing Nothing Nothing (Just "To jest opis") (Just 2014) (Just 2) False (Just wydawca) (Just "Montreal") Artykul (Just "6 str.") JezykPL [klucz2] [haslo1] [link2, link3] now now
    -- kopaut1 <- runDB $ insert $ KopalniaAutor autor1 item1 AutorAut
    -- kopaut2 <- runDB $ insert $ KopalniaAutor autor2 item2 AutorRed
    -- kopaut3 <- runDB $ insert $ KopalniaAutor autor3 item3 AutorTlum
    -- kopaut4 <- runDB $ insert $ KopalniaAutor autor4 item3 AutorWyw
    -- nastepny <- runDB $ insert $ Nastepny 4 2 5 2
    defaultLayout $ do
        setTitle defaultTitle
        $(widgetFile "kopalnia-main")

getKopalniaItemR :: Int64 -> Handler Html
getKopalniaItemR = getKopalniaItemCommon False

getKopalniaItemEditR :: Int64 -> Handler Html
getKopalniaItemEditR = getKopalniaItemCommon True

getKopalniaItemCommon :: Bool -> Int64 -> Handler Html
getKopalniaItemCommon isEdit lookupId = do
    (Entity kopalniaId kopalnia) <- runDB $ getBy404 $ UniqueKopalnia lookupId
    mRodzic <- getMaybe $ kopalniaRodzicId kopalnia
    mNkRodzic <- case mRodzic of
        Just _ -> return Nothing
        Nothing -> getMaybe $ kopalniaNkRodzicId kopalnia
    mDzial <- case mRodzic of
        Just _ -> getMaybe $ kopalniaDzialId kopalnia
        Nothing -> return Nothing
    mWydawca <- getMaybe $ kopalniaWydawcaId kopalnia
    slowaKluczowe <- getListM $ kopalniaSlowaKlucz kopalnia
    haslaPrzedm <- getListM $ kopalniaHaslaPrzedm kopalnia
    linki <- getListM $ kopalniaLinki kopalnia
    -- This call return a List of [Entity KopalniaAutor]
    kopAuts <- runDB $ selectList [KopalniaAutorKopalniaId ==. kopalniaId] []
    -- This call returns a List of Maybe Autor
    allAut <- mapM (\(Entity _ kopAut) -> runDB $ get $ kopalniaAutorAutorId kopAut) kopAuts
    -- Here we walk both lists together and extract only authors of a particular type
    autorzy <- return $ keepOnly AutorAut kopAuts allAut
    redaktorzy <- return $ keepOnly AutorRed kopAuts allAut
    tlumacze <- return $ keepOnly AutorTlum kopAuts allAut
    wywiadowcy <- return $ keepOnly AutorWyw kopAuts allAut
    defaultLayout $
        if isEdit then do
            setTitle $ "Edycja fiszki publikacji - " ++ defaultTitle
            $(widgetFile "kopalnia-item")
        else do
            setTitle "Fiszka publikacji - Polska Bibliografia Wiedzy o Komiksie - Zeszyty Komiksowe"
            $(widgetFile "kopalnia-item")

getMaybe :: (PersistEntity ent, PersistStore (YesodPersistBackend site),
             YesodPersist site,
             PersistEntityBackend ent ~ YesodPersistBackend site) =>
            Maybe (Key ent) -> HandlerT site IO (Maybe ent)
getMaybe (Just lookupId) = runDB $ get lookupId
getMaybe _ = return Nothing

getListM :: (PersistEntity ent, PersistStore (YesodPersistBackend site),
             YesodPersist site,
             PersistEntityBackend ent ~ YesodPersistBackend site) =>
            [Key ent] -> HandlerT site IO [(Maybe ent)]
getListM = mapM (\key -> runDB $ get key)

-- This function is sort of a combination of map, zip and filter.
-- It walks both input lists in parallel and retains only Autor records that are not Nothing
-- and whose corresponding KopalniaAutor have the 'typ' field equal to the first argument.
keepOnly :: TypAutora -> [Entity KopalniaAutor] -> [Maybe Autor] -> [Autor]
keepOnly typ' kas' auts' = keepOnly' typ' kas' auts' []
    where
        -- First make sure that missing authors are skipped.
        keepOnly' typ (_:kas) (Nothing:auts) output = keepOnly' typ kas auts output
        -- Then look inside the KopalniaAutor record to see if the types match or not.
        keepOnly' typ ((Entity _ ka):kas) ((Just aut):auts) output
            | typ == kopalniaAutorTyp ka = keepOnly' typ kas auts (aut:output)
            | otherwise = keepOnly' typ kas auts output
        keepOnly' _ _ _ output = output

getMiesiac :: Maybe Int64 -> Maybe Text
getMiesiac (Just 1) = Just "styczeń"
getMiesiac (Just 2) = Just "luty"
getMiesiac (Just 3) = Just "marzec"
getMiesiac (Just 4) = Just "kwiecień"
getMiesiac (Just 5) = Just "maj"
getMiesiac (Just 6) = Just "czerwiec"
getMiesiac (Just 7) = Just "lipiec"
getMiesiac (Just 8) = Just "sierpień"
getMiesiac (Just 9) = Just "wrzesień"
getMiesiac (Just 10) = Just "październik"
getMiesiac (Just 11) = Just "listopad"
getMiesiac (Just 12) = Just "grudzień"
getMiesiac _ = Nothing

getSlowoKlucz :: Maybe SlowoKlucz -> Text
getSlowoKlucz (Just sk) = slowoKluczSlowo sk
getSlowoKlucz Nothing = ""

getHasloPrzedm :: Maybe HasloPrzedm -> Text
getHasloPrzedm (Just hp) = hasloPrzedmHaslo hp
getHasloPrzedm Nothing = ""

-- The approach used in the functions below is not DRY at all, but at the same time it simplifies 
-- the conditional HTML building a lot.

-- Format:
--   <span class="inobtrusive">W:</span> RodzicId.Tytul, RodzicOpis <span class="inobtrusive">(RodzicId.Rodzaj; dział:</span> DzialId->Tytul <span class="inobtrusive">)</span>
-- Options (RodzicId can be a Kopalnia or a NkPub; NkPubs don't have Dzial): 
--   W: RodzicId, RodzicOpis (RodzajRodzica; dział: DzialId)
--   W: RodzicId, RodzicOpis (RodzajRodzica)
--   W: RodzicId (RodzajRodzica; dział: DzialId)
--   W: RodzicId (RodzajRodzica)
--   W: RodzicOpis
-- Parameters:
--   getRodzicW rodzicOpis rodzic dzial nkRodzic
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

-- Format:
--   <b>Wydawca</b>, MiejsceWyd PubRok
-- Opcje:
--   Wydawca, Miejsce Rok
--   Wydawca, Miejsce
--   Wydawca Rok
--   Wydawca
--   Miejsce Rok
--   Miejsce
getWydawcaW :: Maybe Wydawca -> Maybe Text -> Maybe Int64 -> Widget
getWydawcaW (Just wydawca) (Just miejsce) (Just rok) = 
    toWidget [hamlet|
    <p>
        <b><a href="#" class="link1">#{wydawcaNazwa wydawca}</a>#
        \, #{miejsce} #{show rok}
    |]
getWydawcaW (Just wydawca) (Just miejsce) Nothing = 
    toWidget [hamlet|
    <p>
    <p>
        <b><a href="#" class="link1">#{wydawcaNazwa wydawca}</a>#
        \, #{miejsce}
    |]
getWydawcaW (Just wydawca) Nothing (Just rok) = 
    toWidget [hamlet|
    <p>
    <p>
        <b><a href="#" class="link1">#{wydawcaNazwa wydawca}</a>
        #{show rok}
    |]
getWydawcaW (Just wydawca) Nothing Nothing = 
    toWidget [hamlet|
    <p>
    <p>
        <b><a href="#" class="link1">#{wydawcaNazwa wydawca}</a>
    |]
getWydawcaW Nothing (Just miejsce) (Just rok) = 
    toWidget [hamlet|
    <p>
        #{miejsce} #{show rok}
    |]
getWydawcaW Nothing (Just miejsce) Nothing = 
    toWidget [hamlet|
    <p>
        #{miejsce}
    |]
getWydawcaW _ _ _ = 
    return ()
