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
import Text.Julius (rawJS)
import Utils
import DbUtils
import Handler.KopalniaEdit (fields)
import Handler.KopalniaWidgets
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
    -- Putting this code in the template does not compile
    let wydText = maybe "" (T.pack . show . wydawcaLookupId) mWydawca
    defaultLayout $ do
        setTitle $ prefix ++ " publikacji - " ++ defaultTitle
        $(widgetFile "kopalnia-item")

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

postKopalniaItemUpdateR :: Handler Text
postKopalniaItemUpdateR = do
    (params, _) <- runRequestBody
    let mHandler = lookupEditHandler "name" params fields
    case mHandler of
        Just handler -> handler params
        Nothing -> sendResponseStatus badRequest400 (systemError "Brak funkcji obsługującej albo parametru 'name'")
