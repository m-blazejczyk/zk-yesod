module Enums where

import Database.Persist.TH
import Prelude (Show, Read, Eq)
import Data.Text

data Rodzaj = Pismo | Kolekcja | Numer | Portal | Dzial | Ksiazka | Praca | Artykul | Rozdzial | Wywiad | AudioWideo | Audycja | Notka | RodzajInny
    deriving (Show, Read, Eq)
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

data Jezyk = JezykPL | JezykEN | JezykFR | JezykRU | JezykDE | JezykUA | JezykCZ | JezykSK | JezykLT | JezykSE | JezykES | JezykIT | JezykInny
    deriving (Show, Read, Eq)
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

data TypAutora = AutorAut | AutorRed | AutorTlum | AutorWyw
    deriving (Show, Read, Eq)
derivePersistField "TypAutora"
