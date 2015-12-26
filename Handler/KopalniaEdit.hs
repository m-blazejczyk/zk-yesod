module Handler.KopalniaEdit (
    fields
    ) where

import Import
import Enums
import qualified Data.Text as T
import Network.URI (isURI)
import Utils
import Handler.XEditable

fields :: [(KopalniaField, (Text, EditHandler))]
fields = [(FldTytul,       ("tytul",      editTytulR)),
          (FldRodzaj,      ("rodzaj",     editRodzajR)),
          (FldLinkGlowny,  ("url",        editLinkGlownyR)),
          (FldAutor,       ("autorzy",    editAutorR)),
          (FldTlumacz,     ("tlumacze",   editTlumaczR)),
          (FldRedaktor,    ("redaktorzy", editRedaktorR)),
          (FldWywiad,      ("wywiadowcy", editWywiadR)),
          (FldRodzic,      ("rodzic",     editRodzicR)),
          (FldWydawcy,     ("wydawcy",    editWydawcyR)),
          (FldAddWydawca,  ("addWydawce", editAddWydawcaR)),
          (FldDataWydania, ("dataWyd",    editDataWydaniaR)),
          (FldIsbn,        ("isbn",       editIsbnR)),
          (FldStrony,      ("strony",     editStronyR)),
          (FldObjetosc,    ("objetosc",   editObjetoscR)),
          (FldJezyk,       ("jezyk",      editJezykR)),
          (FldOpis,        ("opis",       editOpisR)),
          (FldHasla,       ("hasla",      editHaslaR)),
          (FldSlowaKlucz,  ("slowa",      editSlowaKluczR))]

editTytulR :: EditHandler
editTytulR = processXEditable1 vald upd where
    vald v | T.length v > 0 = return $ Success v
           | otherwise = return $ Error "Tytuł nie może być pusty"
    upd value = [KopalniaTytul =. value]

editLinkGlownyR :: EditHandler
editLinkGlownyR = processXEditable1 vald upd where
    vald v | T.length v == 0 = return $ Success Nothing
           | isURI $ unpack v = return $ Success $ Just v
           | otherwise = return $ Error "Niepoprawny adres"
    upd value = [KopalniaUrl =. value]

editRodzajR :: EditHandler
editRodzajR = processXEditable1 vald upd where
    vald = return . readRodzaj
    upd value = [KopalniaRodzaj =. value]

editAutorR :: EditHandler
editAutorR = editAutorGenericR AutorAut

editTlumaczR :: EditHandler
editTlumaczR = editAutorGenericR AutorTlum

editRedaktorR :: EditHandler
editRedaktorR = editAutorGenericR AutorRed

editWywiadR :: EditHandler
editWywiadR = editAutorGenericR AutorWyw

editAutorGenericR :: TypAutora -> EditHandler
editAutorGenericR typAutora = processXEditable (valdArr vald) upd where
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
                runDB $ deleteWhere [KopalniaAutorKopalniaId ==. kopalniaId, KopalniaAutorTyp ==. typAutora]
                _ <- mapM (\autorId -> runDB $ insert $ KopalniaAutor autorId kopalniaId typAutora) autorIds
                return $ Success "OK"
            -- This should NEVER happen!
            Nothing -> return $ Success $ systemErrorS "Fiszka o tym identyfikatorze nie istnieje" kopalniaLookupId

editWydawcyR :: EditHandler
editWydawcyR = processXEditable (valdArr vald) upd where
    vald arr = 
        -- mapMaybe :: (Maybe Text -> Maybe Int64) -> [Maybe Text] -> [Int64]
        let arrIds = mapMaybe maybeRead (map Just arr)
        -- mapMaybe filters out all Nothing values from the list so if any id was invalid
        -- then the result will be shorter.
        in if length arrIds == length arr
            then valdDb arrIds
            else return $ Error $ systemError "Niepoprawny identyfikator wydawcy"
    valdDb lookupIds = do
        -- mapM :: (a -> Handler (Maybe (Entity x y))) -> [a] -> Handler [Maybe (Entity x y)]
        mWydawcy <- mapM (\l -> runDB $ getBy $ UniqueWydawca l) lookupIds  -- mWydawcy :: [Maybe (Entity x y)]
        let ids = mapMaybe extractWydawcaId mWydawcy  -- ids :: [x] (see annotation in the line above)
        if length ids == length lookupIds
            then return $ Success $ ids
            else return $ Error $ systemError "Niezdefiniowany identyfikator wydawcy"
    extractWydawcaId (Just (Entity wydawcaId _)) = Just wydawcaId
    extractWydawcaId Nothing = Nothing
    -- 'wydawcaIds' is of type [Key Wydawca]
    upd kopalniaLookupId wydawcaIds = do
        mKopalnia <- runDB $ getBy $ UniqueKopalnia kopalniaLookupId
        case mKopalnia of
            Just (Entity kopalniaId _) -> do
                runDB $ deleteWhere [KopalniaWydKopalniaId ==. kopalniaId]
                _ <- mapM (\wydawcaId -> runDB $ insert $ KopalniaWyd wydawcaId kopalniaId) wydawcaIds
                return $ Success "OK"
            -- This should NEVER happen!
            Nothing -> return $ Success $ systemErrorS "Fiszka o tym identyfikatorze nie istnieje" kopalniaLookupId

editRodzicR :: EditHandler
editRodzicR _ = sendResponseStatus badRequest400 ("This is a message!" :: Text)

-- TODO: Add the new publisher to the drop-down in the XEditable on the page.
editAddWydawcaR :: EditHandler
editAddWydawcaR = processXEditable (valdMap ["nazwa", "url"] vald) upd where
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
            Just (Entity _ nast) -> return $ Error $ systemError "Not implemented..." -- do
                -- wydawca <- runDB $ insert $ Wydawca (intPropValue nast) nazwa url
                -- runDB $ updateWhere [IntPropKey ==. "wydawca"] [IntPropValue =. ((intPropValue nast) + 1)]
                -- runDB $ updateWhere [KopalniaLookupId ==. lookupId] [KopalniaWydawcaId =. (Just wydawca)]
                -- return $ Success "OK"
            Nothing -> return $ Error $ systemError "Brak ustawienia 'wydawca' w bazie danych"

editDataWydaniaR :: EditHandler
editDataWydaniaR = processXEditable (valdMap ["year", "month"] vald) upd where
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
editIsbnR = processXEditable1 vald upd where
    vald = return . vald'
    vald' v | T.length v == 0 = Success Nothing
            | T.length v < 10 = Error "Za krótki kod"
            | T.length v > 17 = Error "Za długi kod"
            | otherwise = Success $ Just v
    upd value = [KopalniaIsbn =. value]

editStronyR :: EditHandler
editStronyR = processXEditable1 vald upd where
    vald v = return $ if T.length v == 0 then Success Nothing else Success $ Just v
    upd value = [KopalniaStrony =. value]

editObjetoscR :: EditHandler
editObjetoscR = processXEditable1 vald upd where
    vald v = return $ if T.length v == 0 then Success Nothing else Success $ Just v
    upd value = [KopalniaObjetosc =. value]

editJezykR :: EditHandler
editJezykR = processXEditable1 vald upd where
    vald = return . readJezyk
    upd value = [KopalniaJezyk =. value]

editOpisR :: EditHandler
editOpisR = processXEditable1 vald upd where
    vald v = return $ if T.length v == 0 then Success Nothing else Success $ Just v
    upd value = [KopalniaOpis =. value]

editHaslaR :: EditHandler
editHaslaR _ = sendResponseStatus badRequest400 ("This is a message!" :: Text)

editSlowaKluczR :: EditHandler
editSlowaKluczR _ = sendResponseStatus badRequest400 ("This is a message!" :: Text)
