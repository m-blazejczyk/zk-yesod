module Handler.SlowoKlucz (getFindSlowoR,
                           slowaToFieldValue
                           ) where

import Import
import Database.Persist.MongoDB ((=~.))
import qualified Data.Text as T
import DbUtils (itemsToFieldValue)
import Handler.Common (processSearchQuery)

getFindSlowoR :: Handler Value
getFindSlowoR = processSearchQuery crit transform
    where
        crit regex = [SlowoKluczSlowo =~. regex]
        transform (Entity _ slowo) = object ["id" .= slowoKluczLookupId slowo, "text" .= slowoKluczSlowo slowo]

slowaToFieldValue :: [SlowoKlucz] -> T.Text
slowaToFieldValue = itemsToFieldValue slowoKluczLookupId slowoKluczSlowo
