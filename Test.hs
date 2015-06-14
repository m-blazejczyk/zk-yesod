{-# LANGUAGE OverloadedStrings #-}

import Prelude
import Data.Text (Text, pack, append)
import Control.Arrow ((&&&))
import Data.Aeson (encode, object, (.=))
import Data.Vector (fromList)

data Rodzaj = Pismo | Kolekcja | Numer | Portal | Dzial | Ksiazka | Praca | Artykul | Rozdzial | Wywiad | AudioWideo | Audycja | Notka | RodzajInny
    deriving (Show, Read, Eq, Enum)

showRodzaj :: Rodzaj -> Text
showRodzaj Pismo =      pack "Czasopismo"
showRodzaj Kolekcja =   pack "Kolekcja wydawnicza"
showRodzaj Numer =      pack "Numer czasopisma"
showRodzaj Portal =     pack "Portal internetowy"
showRodzaj Dzial =      pack "Dział"
showRodzaj Ksiazka =    pack "Książka"
showRodzaj Praca =      pack "Praca naukowa"
showRodzaj Artykul =    pack "Artykuł"
showRodzaj Rozdzial =   pack "Rozdział"
showRodzaj Wywiad =     pack "Wywiad"
showRodzaj AudioWideo = pack "Nagranie audio lub wideo"
showRodzaj Audycja =    pack "Audycja RTV"
showRodzaj Notka =      pack "Notka prasowa"
showRodzaj RodzajInny = pack "Inny"

-- { source: [{value: 'Pismo', text: 'Czasopismo'},
--            {value: 'Kolekcja', text: 'Kolekcja wydawnicza'},
--            {value: 'Numer', text: 'Numer czasopisma'}]}
main :: IO ()
main = do
  let l1 = map ((pack . show) &&& showRodzaj) [Pismo ..]  -- this returns a list of tuples
      formatOne p = object [("value" .= fst p), ("text" .= snd p)]
      formatAll = map formatOne l1
  print $ encode $ object ["source" .= fromList formatAll]
