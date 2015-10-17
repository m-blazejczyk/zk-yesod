module Handler.Kopalnia (
    getKopalniaMainR,
    getKopalniaInitR,
    getKopalniaItemR,
    getKopalniaItemEditR,
    postKopalniaItemUpdateR
    ) where

import Import
import Enums
import qualified Data.Text as T
import Network.URI (isURI)
import Text.Julius (rawJS)
import Utils
import DbUtils
import Handler.XEditable

defaultTitle :: Html
defaultTitle = "Polska Bibliografia Wiedzy o Komiksie - Zeszyty Komiksowe"

getKopalniaMainR :: Handler Html
getKopalniaMainR = do
    defaultLayout $ do
        setTitle defaultTitle
        $(widgetFile "kopalnia-main")

-- This handler is used to put data into a fresh test Kopalnia database.
getKopalniaInitR :: Handler Text
getKopalniaInitR = do
    autor1 <- runDB $ insert $ Autor 1 (Just "Piotr") "Marczewski"
    autor2 <- runDB $ insert $ Autor 2 (Just "Jerzy") "Szyłak"
    autor3 <- runDB $ insert $ Autor 3 (Just "Michał") "Traczyk"
    autor4 <- runDB $ insert $ Autor 4 (Just "Tomasz") "Marciniak"
    now <- liftIO $ getCurrentTime
    nk1 <- runDB $ insert $ NkPub 1 "Polityka" Pismo
    klucz1 <- runDB $ insert $ SlowoKlucz "Klucz 1"
    klucz2 <- runDB $ insert $ SlowoKlucz "Klucz 2"
    haslo1 <- runDB $ insert $ HasloPrzedm "Grafika"
    haslo2 <- runDB $ insert $ HasloPrzedm "Muzyka"
    wydawca <- runDB $ insert $ Wydawca 1 "Timof i cisi wspólnicy" (Just "http://www.timof.pl")
    link1 <- runDB $ insert $ KopalniaLink "http://www.zeszytykomiksowe.org" Nothing Nothing
    link2 <- runDB $ insert $ KopalniaLink "http://www.google.com" (Just "Google") Nothing
    link3 <- runDB $ insert $ KopalniaLink "http://www.pgx.ca" Nothing (Just "Strona PGx")
    item1 <- runDB $ insert $ Kopalnia 1 Nothing Nothing (Just nk1) Nothing Nothing Nothing Nothing "Komiks i jego konteksty" (Just "nk") Nothing (Just "Olo") (Just "kk") Nothing (Just "To jest opis") (Just 1999) Nothing True Nothing Nothing Artykul Nothing JezykPL [] [] [] now now
    item2 <- runDB $ insert $ Kopalnia 2 Nothing Nothing Nothing Nothing (Just "Rodzic") Nothing (Just "1111-111") "Zeszyty Komiksowe" Nothing Nothing Nothing Nothing Nothing (Just "To jest opis") (Just 2007) (Just 10) False (Just wydawca) (Just "Poznań") Pismo (Just "128 str.") JezykPL [klucz1, klucz2] [haslo1, haslo2] [link1] now now
    item3 <- runDB $ insert $ Kopalnia 3 (Just "http://www.zeszytykomiksowe.org") (Just item2) Nothing (Just item1) Nothing (Just "22-27") (Just "2222-111") "Dlaczego nie lubię komiksów" Nothing Nothing Nothing Nothing Nothing (Just "To jest opis") (Just 2014) (Just 2) False (Just wydawca) (Just "Montreal") Artykul (Just "6 str.") JezykPL [klucz2] [haslo1] [link2, link3] now now
    _ <- runDB $ insert $ KopalniaAutor autor1 item1 AutorAut
    _ <- runDB $ insert $ KopalniaAutor autor2 item2 AutorRed
    _ <- runDB $ insert $ KopalniaAutor autor3 item3 AutorTlum
    _ <- runDB $ insert $ KopalniaAutor autor4 item3 AutorWyw
    _ <- runDB $ insert $ IntProp "kopalnia" 4
    _ <- runDB $ insert $ IntProp "nkPub" 2
    _ <- runDB $ insert $ IntProp "autor" 5
    _ <- runDB $ insert $ IntProp "wydawca" 2
    sendResponseStatus status200 ("Baza danych została zainicjalizowana." :: Text)

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
    let prefix = if isEdit then "Edycja fiszki" else "Fiszka"
    defaultLayout $ do
        setTitle $ prefix ++ " publikacji - " ++ defaultTitle
        $(widgetFile "kopalnia-item")

data KopalniaField = 
     FldTytul
   | FldRodzaj
   | FldLinkGlowny
   | FldAutor
   | FldTlumacz
   | FldRedaktor
   | FldWywiad
   | FldRodzic
   | FldWydawca
   | FldAddWydawca
   | FldDataWydania
   | FldIsbn
   | FldStrony
   | FldObjetosc
   | FldJezyk
   | FldOpis
   | FldHasla
   | FldSlowaKlucz
   deriving (Eq)

fields :: [(KopalniaField, (Text, EditHandler))]
fields = [(FldTytul,       ("tytul",      editTytulR)),
          (FldRodzaj,      ("rodzaj",     editRodzajR)),
          (FldLinkGlowny,  ("url",        editLinkGlownyR)),
          (FldAutor,       ("autorzy",    editAutorR)),
          (FldTlumacz,     ("tlumacze",   editTlumaczR)),
          (FldRedaktor,    ("redaktorzy", editRedaktorR)),
          (FldWywiad,      ("wywiadowcy", editWywiadR)),
          (FldRodzic,      ("rodzic",     editRodzicR)),
          (FldWydawca,     ("wydawca",    editWydawcaR)),
          (FldAddWydawca,  ("addWydawce", editAddWydawcaR)),
          (FldDataWydania, ("dataWyd",    editDataWydaniaR)),
          (FldIsbn,        ("isbn",       editIsbnR)),
          (FldStrony,      ("strony",     editStronyR)),
          (FldObjetosc,    ("objetosc",   editObjetoscR)),
          (FldJezyk,       ("jezyk",      editJezykR)),
          (FldOpis,        ("opis",       editOpisR)),
          (FldHasla,       ("hasla",      editHaslaR)),
          (FldSlowaKlucz,  ("slowa",      editSlowaKluczR))]

postKopalniaItemUpdateR :: Handler Text
postKopalniaItemUpdateR = do
    (params, _) <- runRequestBody
    let mHandler = lookupEditHandler "name" params fields
    case mHandler of
        Just handler -> handler params
        Nothing -> sendResponseStatus badRequest400 (systemError "Brak funkcji obsługującej albo parametru 'name'")

editTytulR :: EditHandler
editTytulR params = processXEditable1 params vald upd where
    vald v | T.length v > 0 = return $ Success v
           | otherwise = return $ Error "Tytuł nie może być pusty"
    upd value = [KopalniaTytul =. value]

editLinkGlownyR :: EditHandler
editLinkGlownyR params = processXEditable1 params vald upd where
    vald v | T.length v == 0 = return $ Success Nothing
           | isURI $ unpack v = return $ Success $ Just v
           | otherwise = return $ Error "Niepoprawny adres"
    upd value = [KopalniaUrl =. value]

editRodzajR :: EditHandler
editRodzajR params = processXEditable1 params vald upd where
    vald = return . readRodzaj
    upd value = [KopalniaRodzaj =. value]

editAutorR :: EditHandler
editAutorR params = processXEditable params (valdArr vald) upd where
    vald arr = 
        -- mapMaybe :: (Maybe Text -> Maybe Int64) -> [Maybe Text] -> [Int64]
        let arrIds = mapMaybe maybeRead (map Just arr)
        -- mapMaybe filters out all Nothing values from the list so if any id was invalid
        -- then the result will be shorter.
        in if length arrIds == length arr
            then valdDb arrIds
            else return $ Error $ systemError "Niepoprawny identyfikator autora"
    valdDb lookupIds = do
        -- mapM :: (a -> Handler (Maybe (Entity x y))) -> [a] -> Handler [Maybe (Entity x y)]
        mAutorzy <- mapM (\l -> runDB $ getBy $ UniqueAutor l) lookupIds  -- mAutorzy :: [Maybe (Entity x y)]
        let ids = mapMaybe extractAutorId mAutorzy  -- ids :: [x] (see annotation in the line above)
        if length ids == length lookupIds
            then return $ Success $ ids
            else return $ Error $ systemError "Niezdefiniowany identyfikator autora"
    extractAutorId (Just (Entity autorId _)) = Just autorId
    extractAutorId Nothing = Nothing
    -- 'autorIds' is of type [Key Autor]
    upd kopalniaLookupId autorIds = do
        mKopalnia <- runDB $ getBy $ UniqueKopalnia kopalniaLookupId
        case mKopalnia of
            Just (Entity kopalniaId _) -> do
                runDB $ deleteWhere [KopalniaAutorKopalniaId ==. kopalniaId, KopalniaAutorTyp ==. AutorAut]
                _ <- mapM (\autorId -> runDB $ insert $ KopalniaAutor autorId kopalniaId AutorAut) autorIds
                return $ Success "OK"
            -- This should NEVER happen!
            Nothing -> return $ Success $ systemErrorS "Fiszka o tym identyfikatorze nie istnieje" kopalniaLookupId

editTlumaczR :: EditHandler
editTlumaczR _ = sendResponseStatus badRequest400 ("This is a message!" :: Text)

editRedaktorR :: EditHandler
editRedaktorR _ = sendResponseStatus badRequest400 ("This is a message!" :: Text)

editWywiadR :: EditHandler
editWywiadR _ = sendResponseStatus badRequest400 ("This is a message!" :: Text)

editRodzicR :: EditHandler
editRodzicR _ = sendResponseStatus badRequest400 ("This is a message!" :: Text)

editWydawcaR :: EditHandler
editWydawcaR params = processXEditable1 params vald upd where
    vald v | T.length v == 0 = return $ Success Nothing
           | otherwise = parseId v
    parseId v = case (maybeRead $ Just v) of
        Just iden -> do
            mWyd <- runDB $ getBy $ UniqueWydawca iden
            processWyd mWyd
        Nothing -> return $ Error $ systemError "Identyfikator wydawcy nie jest liczbą"
    processWyd (Just (Entity wydId _)) = return $ Success $ Just wydId
    processWyd Nothing = return $ Error $ systemError "Niezdefiniowany identyfikator wydawcy"
    upd value = [KopalniaWydawcaId =. value]

-- TODO: Add the new publisher to the drop-down in the XEditable on the page.
editAddWydawcaR :: EditHandler
editAddWydawcaR params = processXEditable params (valdMap ["nazwa", "url"] vald) upd where
    -- TODO: verify this logic
    vald [Just tNazwa, Just tUrl]
        | T.length tNazwa == 0 && T.length tUrl == 0 = return $ Success Nothing
        | T.length tNazwa == 0 && T.length tUrl > 0 = return $ Error "Nazwa wydawcy jest wymagana"
        | T.length tUrl > 0 && (not $ isURI $ unpack tUrl) = return $ Error "Niepoprawny adres strony internetowej"
        | T.length tUrl == 0 = return $ Success $ Just (tNazwa, Nothing)
        | otherwise = return $ Success $ Just (tNazwa, Just tUrl)
    vald _ = return $ Error $ systemError "Niepoprawna ilość parametrów"
    upd _ Nothing = return $ Success "IGNORE"  -- If both parameters were empty, simply ignore the request.
                                               -- Don't change this text without changing the JS file.
    upd lookupId (Just (nazwa, url)) = do
        mNast <- runDB $ getBy $ UniqueIntProp "wydawca"
        case mNast of
            Just (Entity _ nast) -> do
                wydawca <- runDB $ insert $ Wydawca (intPropValue nast) nazwa url
                runDB $ updateWhere [IntPropKey ==. "wydawca"] [IntPropValue =. ((intPropValue nast) + 1)]
                runDB $ updateWhere [KopalniaLookupId ==. lookupId] [KopalniaWydawcaId =. (Just wydawca)]
                return $ Success "OK"
            Nothing -> return $ Error $ systemError "Brak ustawienia 'wydawca' w bazie danych"

editDataWydaniaR :: EditHandler
editDataWydaniaR params = processXEditable params (valdMap ["year", "month"] vald) upd where
    vald [Just tYear, Just tMonth] = do
        curDate <- liftIO (getCurrentTime >>= return . toGregorian . utctDay)
        curYear <- return $ fromIntegral $ fst3 curDate
        validated <- return $ [valdYear curYear tYear, valdMonth tMonth]
        validated' <- return $ crossValidate validated
        combined <- return $ combine "\n" validated'
        case combined of
            Success (year:month:_) -> return $ Success (year, month)
            Error err -> return $ Error err
            _ -> return $ Error $ systemError "Niepoprawna ilość parametrów"
    vald _ = return $ Error $ systemError "Niepoprawna ilość parametrów"
    valdYear curYear tYear = if tYear == "" then Success Nothing else case maybeRead $ Just tYear of
        Just year -> if year < 1850 || year > curYear + 1 then Error "Niepoprawny rok" else Success $ Just year
        Nothing -> Error "Rok nie jest liczbą"
    valdMonth tMonth = if tMonth == "" then Success Nothing else case maybeRead $ Just tMonth of
        Just month -> if month < 1 || month > 12 then Error "Niepoprawny miesiąc" else Success $ Just month
        Nothing -> Error "Miesiąc nie jest liczbą"
    crossValidate v@[Success Nothing, Success (Just _)] = v ++ [Error "Rok jest wymagany, jeśli podajesz miesiąc"]
    crossValidate v = v
    upd lookupId value = do
        let criterion = KopalniaLookupId ==. lookupId
        runDB $ updateWhere [criterion] [KopalniaPubRok =. fst value]
        runDB $ updateWhere [criterion] [KopalniaPubMiesiac =. snd value]
        return $ Success "OK"

-- TODO: add regex validation
editIsbnR :: EditHandler
editIsbnR params = processXEditable1 params vald upd where
    vald = return . vald'
    vald' v | T.length v == 0 = Success Nothing
            | T.length v < 10 = Error "Za krótki kod"
            | T.length v > 17 = Error "Za długi kod"
            | otherwise = Success $ Just v
    upd value = [KopalniaIsbn =. value]

editStronyR :: EditHandler
editStronyR params = processXEditable1 params vald upd where
    vald v = return $ if T.length v == 0 then Success Nothing else Success $ Just v
    upd value = [KopalniaStrony =. value]

editObjetoscR :: EditHandler
editObjetoscR params = processXEditable1 params vald upd where
    vald v = return $ if T.length v == 0 then Success Nothing else Success $ Just v
    upd value = [KopalniaObjetosc =. value]

editJezykR :: EditHandler
editJezykR params = processXEditable1 params vald upd where
    vald = return . readJezyk
    upd value = [KopalniaJezyk =. value]

editOpisR :: EditHandler
editOpisR params = processXEditable1 params vald upd where
    vald v = return $ if T.length v == 0 then Success Nothing else Success $ Just v
    upd value = [KopalniaOpis =. value]

editHaslaR :: EditHandler
editHaslaR _ = sendResponseStatus badRequest400 ("This is a message!" :: Text)

editSlowaKluczR :: EditHandler
editSlowaKluczR _ = sendResponseStatus badRequest400 ("This is a message!" :: Text)

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
