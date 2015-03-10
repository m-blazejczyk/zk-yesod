module Enums where

import Database.Persist.TH
import Prelude (Show, show, Read, Eq)

data Rodzaj = Pismo | Kolekcja | Numer | Portal | Dzial | Ksiazka | Praca | Artykul | Rozdzial | Wywiad | AudioWideo | Audycja | Notka | RodzajInny
    deriving (Read, Eq)
derivePersistField "Rodzaj"

instance Show Rodzaj where  
    show Pismo =      "Czasopismo"
    show Kolekcja =   "Kolekcja wydawnicza"
    show Numer =      "Numer czasopisma"
    show Portal =     "Portal internetowy"
    show Dzial =      "Dział"
    show Ksiazka =    "Książka"
    show Praca =      "Praca naukowa"
    show Artykul =    "Artykuł"
    show Rozdzial =   "Rozdział"
    show Wywiad =     "Wywiad"
    show AudioWideo = "Nagranie audio lub wideo"
    show Audycja =    "Audycja RTV"
    show Notka =      "Notka prasowa"
    show RodzajInny = "Inny"

data Jezyk = JezykPL | JezykEN | JezykFR | JezykRU | JezykDE | JezykUA | JezykCZ | JezykSK | JezykLT | JezykSE | JezykES | JezykIT | JezykInny
    deriving (Read, Eq)
derivePersistField "Jezyk"

instance Show Jezyk where
    show JezykPL =   "polski"
    show JezykEN =   "angielski"
    show JezykFR =   "francuski"
    show JezykRU =   "rosyjski"
    show JezykDE =   "niemiecki"
    show JezykUA =   "ukraiński"
    show JezykCZ =   "czeski"
    show JezykSK =   "słowacki"
    show JezykLT =   "litewski"
    show JezykSE =   "szwedzki"
    show JezykES =   "hiszpański"
    show JezykIT =   "włoski"
    show JezykInny = "Inny"

data TypAutora = AutorAut | AutorRed | AutorTlum | AutorWyw
    deriving (Show, Read, Eq)
derivePersistField "TypAutora"
