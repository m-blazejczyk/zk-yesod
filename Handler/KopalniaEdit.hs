module Handler.KopalniaEdit (
    fields
    ) where

import Import
import Enums
import qualified Data.Text as T
import Network.URI (isURI)
import Utils
import DbUtils (dbPropWydawca, dbPropHaslo, dbPropSlowo, updateTagTable)
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
editAutorGenericR typAutora = processXEditableMulti getUnique extractId delFilter insRecord undefined "Autor" where
    getUnique = UniqueAutor
    extractId (Just (Entity autorId _)) = Just autorId
    extractId Nothing = Nothing
    delFilter kopalniaId = [KopalniaAutorKopalniaId ==. kopalniaId, KopalniaAutorTyp ==. typAutora]
    insRecord autorId kopalniaId = KopalniaAutor autorId kopalniaId typAutora

editWydawcyR :: EditHandler
editWydawcyR = processXEditableMulti getUnique extractId delFilter insRecord undefined "Wydawca" where
    getUnique = UniqueWydawca
    extractId (Just (Entity wydawcaId _)) = Just wydawcaId
    extractId Nothing = Nothing
    delFilter kopalniaId = [KopalniaWydKopalniaId ==. kopalniaId]
    insRecord = KopalniaWyd

editRodzicR :: EditHandler
editRodzicR _ = sendResponseStatus badRequest400 ("editRodzicR nie zaimplementowany" :: Text)

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
        mNast <- runDB $ getBy $ UniqueIntProp dbPropWydawca
        case mNast of
            Just (Entity _ nast) -> do
                mKopalnia <- runDB $ getBy $ UniqueKopalnia lookupId
                case mKopalnia of
                    Just (Entity kopalniaId _) -> do
                        wydawcaId <- updateTagTable nast
                                                    dbPropWydawca 
                                                    (\lid -> Wydawca lid nazwa url)
                                                    (\lid -> KopalniaWyd lid kopalniaId)
                        -- Important: we need to return the inserted publisher's ID as plain text from the POST request.
                        -- See how onAdd() is called in editable-zk.js.
                        return $ Success $ (T.pack . show) wydawcaId
                    -- This should NEVER happen!
                    Nothing -> return $ Error $ systemError "Niepoprawny identyfikator kopalni"
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
editHaslaR = processXEditableMulti getUnique extractId delFilter insRecord processNewItem "HasloPrzedm" where
    getUnique = UniqueHasloPrzedm
    extractId (Just (Entity hasloId _)) = Just hasloId
    extractId Nothing = Nothing
    delFilter hasloId = [KopalniaHasloKopalniaId ==. hasloId]
    insRecord = KopalniaHaslo
    processNewItem kopalniaId item = do
        mNast <- runDB $ getBy $ UniqueIntProp dbPropHaslo
        case mNast of
            Just (Entity _ nast) -> do
                hasloId <- updateTagTable nast
                                          dbPropHaslo 
                                          (\lookupId -> HasloPrzedm lookupId item)
                                          (\lookupId -> KopalniaHaslo lookupId kopalniaId)
                return $ Just hasloId
            Nothing -> return Nothing

editSlowaKluczR :: EditHandler
editSlowaKluczR = processXEditableMulti getUnique extractId delFilter insRecord processNewItem "SlowoKlucz" where
    getUnique = UniqueSlowoKlucz
    extractId (Just (Entity slowoId _)) = Just slowoId
    extractId Nothing = Nothing
    delFilter slowoId = [KopalniaSlowoKopalniaId ==. slowoId]
    insRecord = KopalniaSlowo
    processNewItem kopalniaId item = do
        mNast <- runDB $ getBy $ UniqueIntProp dbPropSlowo
        case mNast of
            Just (Entity _ nast) -> do
                slowoId <- updateTagTable nast
                                          dbPropSlowo
                                          (\lookupId -> SlowoKlucz lookupId item)
                                          (\lookupId -> KopalniaSlowo lookupId kopalniaId)
                return $ Just slowoId
            Nothing -> return Nothing
