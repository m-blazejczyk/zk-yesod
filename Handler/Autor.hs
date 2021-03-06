module Handler.Autor (getAutorR,
                      getFindAutorR,
                      autorzyToFieldValue
                      ) where

import Import
import Database.Persist.MongoDB ((=~.))
import qualified Data.Text as T
import DbUtils (itemsToFieldValue)
import Handler.Common (processSearchQuery)

getAutorR :: Int64 -> Handler Html
getAutorR lookupId = do
    (Entity _ autor) <- runDB $ getBy404 $ UniqueAutor lookupId
    defaultLayout $ do
        setTitle "Fiszka autora - Polska Bibliografia Wiedzy o Komiksie - Zeszyty Komiksowe"
        $(widgetFile "autor")

getFindAutorR :: Handler Value
getFindAutorR = processSearchQuery crit transform
    where
        crit regex = [AutorImiona =~. regex] ||. [AutorNazwisko =~. regex]
        transform (Entity _ autor) = object ["id" .= autorLookupId autor, "text" .= getName autor]
        getName autor = T.concat [getFName autor, autorNazwisko autor]
        getFName autor = maybe "" (\i -> T.concat [i, " "]) (autorImiona autor)

autorzyToFieldValue :: [Autor] -> T.Text
autorzyToFieldValue = itemsToFieldValue autorLookupId autorNazwa
    where autorNazwa aut = T.intercalate " " (catMaybes [autorImiona aut, Just (autorNazwisko aut)])
