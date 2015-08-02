module Handler.Kopalnia (
    getKopalniaMainR,
    getKopalniaItemR,
    getKopalniaItemEditR,
    postKopalniaEditTytulR,
    postKopalniaEditRodzajR,
    postKopalniaEditLinkGlR,
    postKopalniaEditAutorR,
    postKopalniaEditTlumR,
    postKopalniaEditRedR,
    postKopalniaEditWywiadR,
    postKopalniaEditRodzicR,
    postKopalniaEditWydawcaR,
    postKopalniaEditDataWydR,
    postKopalniaEditIsbnR,
    postKopalniaEditStrR,
    postKopalniaEditObjR,
    postKopalniaEditJezykR,
    postKopalniaEditOpisR,
    postKopalniaEditHaslaR,
    postKopalniaEditSlowaKluczR
    ) where

import Import
import Enums
import qualified Data.Text as T
import Network.URI (isURI)
import Text.Julius (rawJS)
import Utils
import DbUtils

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
    wydawcyJson <- wydawcyToJson
    dataWyd <- return $ (maybe ("" :: String) show (kopalniaPubRok kopalnia)
                       , maybe ("" :: String) show (kopalniaPubMiesiac kopalnia))
    defaultLayout $
        if isEdit then do
            setTitle $ "Edycja fiszki publikacji - " ++ defaultTitle
            $(widgetFile "kopalnia-item")
        else do
            setTitle $ "Fiszka publikacji - " ++ defaultTitle
            $(widgetFile "kopalnia-item")

postKopalniaEditTytulR :: Handler Text
postKopalniaEditTytulR = processXEditable vald upd [] where
    vald _ v | T.length v > 0 = return $ Right v
             | otherwise = return $ Left "Tytuł nie może być pusty"
    upd _ value = [KopalniaTytul =. value]

postKopalniaEditLinkGlR :: Handler Text
postKopalniaEditLinkGlR = processXEditable vald upd [] where
    vald _ v | T.length v == 0 = return $ Right Nothing
             | isURI $ unpack v = return $ Right $ Just v
             | otherwise = return $ Left "Niepoprawny adres"
    upd _ value = [KopalniaUrl =. value]

postKopalniaEditRodzajR :: Handler Text
postKopalniaEditRodzajR = processXEditable vald upd [] where
    vald _ = return . readRodzaj
    upd _ value = [KopalniaRodzaj =. value]

postKopalniaEditAutorR :: Handler Text
postKopalniaEditAutorR = sendResponseStatus badRequest400 ("This is a message!" :: Text)

postKopalniaEditTlumR :: Handler Text
postKopalniaEditTlumR = sendResponseStatus badRequest400 ("This is a message!" :: Text)

postKopalniaEditRedR :: Handler Text
postKopalniaEditRedR = sendResponseStatus badRequest400 ("This is a message!" :: Text)

postKopalniaEditWywiadR :: Handler Text
postKopalniaEditWywiadR = sendResponseStatus badRequest400 ("This is a message!" :: Text)

postKopalniaEditRodzicR :: Handler Text
postKopalniaEditRodzicR = sendResponseStatus badRequest400 ("This is a message!" :: Text)

postKopalniaEditWydawcaR :: Handler Text
postKopalniaEditWydawcaR = processXEditable vald upd [] where
    vald _ v | T.length v == 0 = return $ Right Nothing
             | otherwise = parseId v
    parseId v = case (maybeRead $ Just v) of
        Just iden -> do
            mWyd <- runDB $ getBy $ UniqueWydawca iden
            processWyd mWyd
        Nothing -> return $ Left "Błąd systemu: identyfikator wydawcy nie jest liczbą"
    processWyd (Just (Entity wydId _)) = return $ Right $ Just wydId
    processWyd Nothing = return $ Left "Błąd systemu: niezdefiniowany identyfikator wydawcy"
    upd _ value = [KopalniaWydawcaId =. value]

postKopalniaEditDataWydR :: Handler Text
postKopalniaEditDataWydR = sendResponseStatus status200 ("OK" :: Text)

-- TODO: add regex validation
postKopalniaEditIsbnR :: Handler Text
postKopalniaEditIsbnR = processXEditable vald upd [] where
    vald _ = return . vald'
    vald' v | T.length v == 0 = Right Nothing
            | T.length v < 10 = Left "Za krótki kod"
            | T.length v > 17 = Left "Za długi kod"
            | otherwise = Right $ Just v
    upd _ value = [KopalniaIsbn =. value]

postKopalniaEditStrR :: Handler Text
postKopalniaEditStrR = processXEditable vald upd [] where
    vald _ v = return $ if T.length v == 0 then Right Nothing else Right $ Just v
    upd _ value = [KopalniaStrony =. value]

postKopalniaEditObjR :: Handler Text
postKopalniaEditObjR = processXEditable vald upd [] where
    vald _ v = return $ if T.length v == 0 then Right Nothing else Right $ Just v
    upd _ value = [KopalniaObjetosc =. value]

postKopalniaEditJezykR :: Handler Text
postKopalniaEditJezykR = processXEditable vald upd [] where
    vald _ = return . readJezyk
    upd _ value = [KopalniaJezyk =. value]

postKopalniaEditOpisR :: Handler Text
postKopalniaEditOpisR = processXEditable vald upd [] where
    vald _ v = return $ if T.length v == 0 then Right Nothing else Right $ Just v
    upd _ value = [KopalniaOpis =. value]

postKopalniaEditHaslaR :: Handler Text
postKopalniaEditHaslaR = sendResponseStatus badRequest400 ("This is a message!" :: Text)

postKopalniaEditSlowaKluczR :: Handler Text
postKopalniaEditSlowaKluczR = sendResponseStatus badRequest400 ("This is a message!" :: Text)

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

getBiblioFooterW :: Widget
getBiblioFooterW = toWidget [hamlet|
    <div class="biblio-footer">
      <div class="row">
        <div class="col-md-2 right">
          <a href="http://www.mkidn.gov.pl/" target="_blank"><img src=@{StaticR img_mkidn_biblio_png} width="118" height="70">
        <div class="col-md-2">
          <strong>Dofinansowano ze środków Ministerstwa Kultury i Dziedzictwa Narodowego.
        <div class="col-md-2 right">
          <a href="http://lib.amu.edu.pl/" target="_blank"><img src=@{StaticR img_bu_logo_biblio_png} width="98" height="70">
        <div class="col-md-2">
          Projekt wspierany przez Bibliotekę Uniwersytecką w Poznaniu.
        <div class="col-md-2 right">
          <a href="http://fundacja-ikp.pl/" target="_blank"><img src=@{StaticR img_ikp_logo_biblio_png} width="147" height="63">
        <div class="col-md-2">
          Projekt realizowany we współpracy z Fundacją Instytut Kultury Popularnej.
    |]
