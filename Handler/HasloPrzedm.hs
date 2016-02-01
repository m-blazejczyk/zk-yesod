module Handler.HasloPrzedm (getFindHasloR,
                            haslaToFieldValue
                            ) where

import Import
import Database.Persist.MongoDB ((=~.))
import qualified Data.Text as T
import DbUtils (itemsToFieldValue)
import Handler.Common (processSearchQuery)

getFindHasloR :: Handler Value
getFindHasloR = processSearchQuery crit transform
    where
        crit regex = [HasloPrzedmHaslo =~. regex]
        transform (Entity _ haslo) = object ["id" .= hasloPrzedmLookupId haslo, "text" .= hasloPrzedmHaslo haslo]

haslaToFieldValue :: [HasloPrzedm] -> T.Text
haslaToFieldValue = itemsToFieldValue hasloPrzedmLookupId hasloPrzedmHaslo
