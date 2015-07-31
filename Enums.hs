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

readRodzaj :: Text -> Either Text Rodzaj
readRodzaj "Pismo" = Right Pismo
readRodzaj "Kolekcja" = Right Kolekcja
readRodzaj "Numer" = Right Numer
readRodzaj "Portal" = Right Portal
readRodzaj "Dzial" = Right Dzial
readRodzaj "Ksiazka" = Right Ksiazka
readRodzaj "Praca" = Right Praca
readRodzaj "Artykul" = Right Artykul
readRodzaj "Rozdzial" = Right Rozdzial
readRodzaj "Wywiad" = Right Wywiad
readRodzaj "AudioWideo" = Right AudioWideo
readRodzaj "Audycja" = Right Audycja
readRodzaj "Notka" = Right Notka
readRodzaj "RodzajInny" = Right RodzajInny
readRodzaj _ = Left "Niepoprawny rodzaj publikacji"

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

readJezyk :: Text -> Either Text Jezyk
readJezyk "JezykPL" = Right JezykPL
readJezyk "JezykEN" = Right JezykEN
readJezyk "JezykFR" = Right JezykFR
readJezyk "JezykRU" = Right JezykRU
readJezyk "JezykDE" = Right JezykDE
readJezyk "JezykUA" = Right JezykUA
readJezyk "JezykCZ" = Right JezykCZ
readJezyk "JezykSK" = Right JezykSK
readJezyk "JezykLT" = Right JezykLT
readJezyk "JezykSE" = Right JezykSE
readJezyk "JezykES" = Right JezykES
readJezyk "JezykIT" = Right JezykIT
readJezyk "JezykInny" = Right JezykInny
readJezyk _ = Left "Niepoprawny język publikacji"

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
