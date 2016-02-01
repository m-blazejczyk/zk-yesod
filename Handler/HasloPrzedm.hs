module Handler.HasloPrzedm (getFindHasloR
                            ) where

import Import
import Database.Persist.MongoDB ((=~.))
import Handler.Common (processSearchQuery)

getFindHasloR :: Handler Value
getFindHasloR = processSearchQuery crit transform
    where
        crit regex = [HasloPrzedmHaslo =~. regex]
        transform (Entity _ haslo) = object ["id" .= hasloPrzedmLookupId haslo, "text" .= hasloPrzedmHaslo haslo]
