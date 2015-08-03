module Enums where

import Database.Persist.TH
import Prelude
import Data.Int (Int64)
import Data.Aeson (encode)
import Data.ByteString.Lazy (ByteString)
import Data.Text
import Control.Arrow ((&&&))
import Utils

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

data Rodzaj = Pismo | Kolekcja | Numer | Portal | Dzial | Ksiazka | Praca | Artykul | Rozdzial | Wywiad | AudioWideo | Audycja | Notka | RodzajInny
    deriving (Show, Read, Eq, Enum)
derivePersistField "Rodzaj"

showRodzaj :: Rodzaj -> Text
showRodzaj Pismo =      "Czasopismo"
showRodzaj Kolekcja =   "Kolekcja wydawnicza"
showRodzaj Numer =      "Numer czasopisma"
showRodzaj Portal =     "Portal internetowy"
showRodzaj Dzial =      "Dział"
showRodzaj Ksiazka =    "Książka"
showRodzaj Praca =      "Praca naukowa"
showRodzaj Artykul =    "Artykuł"
showRodzaj Rozdzial =   "Rozdział"
showRodzaj Wywiad =     "Wywiad"
showRodzaj AudioWideo = "Nagranie audio lub wideo"
showRodzaj Audycja =    "Audycja RTV"
showRodzaj Notka =      "Notka prasowa"
showRodzaj RodzajInny = "Inny"

readRodzaj :: Text -> Result Rodzaj
readRodzaj "Pismo" = Success Pismo
readRodzaj "Kolekcja" = Success Kolekcja
readRodzaj "Numer" = Success Numer
readRodzaj "Portal" = Success Portal
readRodzaj "Dzial" = Success Dzial
readRodzaj "Ksiazka" = Success Ksiazka
readRodzaj "Praca" = Success Praca
readRodzaj "Artykul" = Success Artykul
readRodzaj "Rozdzial" = Success Rozdzial
readRodzaj "Wywiad" = Success Wywiad
readRodzaj "AudioWideo" = Success AudioWideo
readRodzaj "Audycja" = Success Audycja
readRodzaj "Notka" = Success Notka
readRodzaj "RodzajInny" = Success RodzajInny
readRodzaj _ = Error "Niepoprawny rodzaj publikacji"

data Jezyk = JezykPL | JezykEN | JezykFR | JezykRU | JezykDE | JezykUA | JezykCZ | JezykSK | JezykLT | JezykSE | JezykES | JezykIT | JezykInny
    deriving (Show, Read, Eq, Enum)
derivePersistField "Jezyk"

showJezyk :: Jezyk -> Text
showJezyk JezykPL =   "polski"
showJezyk JezykEN =   "angielski"
showJezyk JezykFR =   "francuski"
showJezyk JezykRU =   "rosyjski"
showJezyk JezykDE =   "niemiecki"
showJezyk JezykUA =   "ukraiński"
showJezyk JezykCZ =   "czeski"
showJezyk JezykSK =   "słowacki"
showJezyk JezykLT =   "litewski"
showJezyk JezykSE =   "szwedzki"
showJezyk JezykES =   "hiszpański"
showJezyk JezykIT =   "włoski"
showJezyk JezykInny = "Inny"

readJezyk :: Text -> Result Jezyk
readJezyk "JezykPL" = Success JezykPL
readJezyk "JezykEN" = Success JezykEN
readJezyk "JezykFR" = Success JezykFR
readJezyk "JezykRU" = Success JezykRU
readJezyk "JezykDE" = Success JezykDE
readJezyk "JezykUA" = Success JezykUA
readJezyk "JezykCZ" = Success JezykCZ
readJezyk "JezykSK" = Success JezykSK
readJezyk "JezykLT" = Success JezykLT
readJezyk "JezykSE" = Success JezykSE
readJezyk "JezykES" = Success JezykES
readJezyk "JezykIT" = Success JezykIT
readJezyk "JezykInny" = Success JezykInny
readJezyk _ = Error "Niepoprawny język publikacji"

data TypAutora = AutorAut | AutorRed | AutorTlum | AutorWyw
    deriving (Show, Read, Eq)
derivePersistField "TypAutora"

-- To convert a ByteString to Text use decodeUtf8 from Data.Text
enumToJson :: Show e => [e] -> (e -> Text) -> ByteString
enumToJson allEnum showEnum =
    encode $ tuplesToRawJson "source" "value" "text" (Prelude.map ((pack . show) &&& showEnum) allEnum)  -- this returns a list of tuples

rodzajeToJson :: ByteString
rodzajeToJson = enumToJson [Pismo ..] showRodzaj

jezykiToJson :: ByteString
jezykiToJson = enumToJson [JezykPL ..] showJezyk
