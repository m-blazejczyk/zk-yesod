module Handler.NkPub (getFindNkPubR,
                      nkPubToFieldValue
                      ) where

import Import
import Database.Persist.MongoDB ((=~.))
import qualified Data.Text as T
import DbUtils (itemsToFieldValue)
import Handler.Common (processSearchQuery)

getFindNkPubR :: Handler Value
getFindNkPubR = processSearchQuery crit transform
    where
        crit regex = [NkPubTytul =~. regex]
        transform (Entity _ nk) = object ["id" .= nkPubLookupId nk, "text" .= nkPubTytul nk]

nkPubToFieldValue :: NkPub -> T.Text
nkPubToFieldValue nk = itemsToFieldValue nkPubLookupId nkPubTytul [nk]
