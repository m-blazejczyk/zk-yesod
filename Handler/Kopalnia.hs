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
    slowo1 <- runDB $ insert $ SlowoKlucz "Klucz 1"
    slowo2 <- runDB $ insert $ SlowoKlucz "Klucz 2"
    haslo1 <- runDB $ insert $ HasloPrzedm "Grafika"
    haslo2 <- runDB $ insert $ HasloPrzedm "Muzyka"
    wydawca1 <- runDB $ insert $ Wydawca 1 "Timof i cisi wspólnicy" (Just "http://www.timof.pl")
    wydawca2 <- runDB $ insert $ Wydawca 2 "Biblioteka Uniwersytecka" Nothing
    link1 <- runDB $ insert $ KopalniaLink "http://www.zeszytykomiksowe.org" Nothing Nothing
    link2 <- runDB $ insert $ KopalniaLink "http://www.google.com" (Just "Google") Nothing
    link3 <- runDB $ insert $ KopalniaLink "http://www.pgx.ca" Nothing (Just "Strona PGx")
    item1 <- runDB $ insert $ Kopalnia 1 Nothing Nothing (Just nk1) Nothing Nothing Nothing Nothing "Komiks i jego konteksty" (Just "nk") Nothing (Just "Olo") (Just "kk") Nothing (Just "To jest opis") (Just 1999) Nothing True Nothing Artykul Nothing JezykPL [] now now
    item2 <- runDB $ insert $ Kopalnia 2 Nothing Nothing Nothing Nothing (Just "Rodzic") Nothing (Just "1111-111") "Zeszyty Komiksowe" Nothing Nothing Nothing Nothing Nothing (Just "To jest opis") (Just 2007) (Just 10) False (Just "Poznań") Pismo (Just "128 str.") JezykPL [link1] now now
    item3 <- runDB $ insert $ Kopalnia 3 (Just "http://www.zeszytykomiksowe.org") (Just item2) Nothing (Just item1) Nothing (Just "22-27") (Just "2222-111") "Dlaczego nie lubię komiksów" Nothing Nothing Nothing Nothing Nothing (Just "To jest opis") (Just 2014) (Just 2) False (Just "Montreal") Artykul (Just "6 str.") JezykPL [link2, link3] now now
    _ <- runDB $ insert $ KopalniaAutor autor1 item1 AutorAut
    _ <- runDB $ insert $ KopalniaAutor autor2 item2 AutorRed
    _ <- runDB $ insert $ KopalniaAutor autor3 item3 AutorTlum
    _ <- runDB $ insert $ KopalniaAutor autor4 item3 AutorWyw
    _ <- runDB $ insert $ KopalniaWyd wydawca2 item1
    _ <- runDB $ insert $ KopalniaWyd wydawca1 item3
    _ <- runDB $ insert $ KopalniaWyd wydawca2 item3
    _ <- runDB $ insert $ KopalniaSlowo slowo1 item1
    _ <- runDB $ insert $ KopalniaSlowo slowo2 item2
    _ <- runDB $ insert $ KopalniaSlowo slowo1 item3
    _ <- runDB $ insert $ KopalniaSlowo slowo2 item3
    _ <- runDB $ insert $ KopalniaHaslo haslo2 item1
    _ <- runDB $ insert $ KopalniaHaslo haslo1 item2
    _ <- runDB $ insert $ KopalniaHaslo haslo2 item2
    _ <- runDB $ insert $ IntProp "kopalnia" 4
    _ <- runDB $ insert $ IntProp "nkPub" 2
    _ <- runDB $ insert $ IntProp "autor" 5
    _ <- runDB $ insert $ IntProp "wydawca" 3
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
    slowaKluczowe <- getListMany2Many [KopalniaSlowoKopalniaId ==. kopalniaId] kopalniaSlowoSlowoId
    haslaPrzedm <- getListMany2Many [KopalniaHasloKopalniaId ==. kopalniaId] kopalniaHasloHasloId
    wydawcy <- getListMany2Many [KopalniaWydKopalniaId ==. kopalniaId] kopalniaWydWydawcaId
    (kopAuts, allAut) <- getListMany2ManyEx [KopalniaAutorKopalniaId ==. kopalniaId] kopalniaAutorAutorId
    linki <- getListM $ kopalniaLinki kopalnia
    wydawcyJson <- wydawcyToJson
    dataWyd <- return $ (maybe ("" :: String) show (kopalniaPubRok kopalnia)
                       , maybe ("" :: String) show (kopalniaPubMiesiac kopalnia))
    let prefix = if isEdit then "Edycja fiszki" else "Fiszka"
    defaultLayout $ do
        setTitle $ prefix ++ " publikacji - " ++ defaultTitle
        $(widgetFile "kopalnia-item")

-- This function is sort of a combination of map, zip and filter.
-- It walks both input lists in parallel and retains only Autor records that are not Nothing
-- and whose corresponding KopalniaAutor have the 'typ' field equal to the first argument.
keepOnly :: TypAutora -> [Entity KopalniaAutor] -> [Maybe Autor] -> [Autor]
keepOnly typ kas auts = (catMaybes . map snd . filter flt . zip kas) auts
    where flt (_, Nothing) = False
          flt ((Entity _ ka), _)
              | typ == kopalniaAutorTyp ka = True
              | otherwise = False

postKopalniaItemUpdateR :: Handler Text
postKopalniaItemUpdateR = do
    (params, _) <- runRequestBody
    let mHandler = lookupEditHandler "name" params fields
    case mHandler of
        Just handler -> handler params
        Nothing -> sendResponseStatus badRequest400 (systemError "Brak funkcji obsługującej albo parametru 'name'")
