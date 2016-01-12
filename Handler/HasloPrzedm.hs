module Handler.HasloPrzedm (getFindHasloR
                            ) where

import Import
import DbUtils (prefixRegex)
import Utils (systemError)
import Database.Persist.MongoDB ((=~.))
import qualified Data.Vector as V
import qualified Data.Text as T

-- Expected format:
-- {
--   "more": false,
--   "results": [
--     {id: 'abc', text: 'abc'},
--     {id: 'def', text: 'def'},
--     {id: 'ghi', text: 'ghi'}
--   ]
-- }
getFindHasloR :: Handler Value
getFindHasloR = do
    mQ <- lookupGetParam "q"
    case mQ of
        Just q -> do
            let autJson = fmap transform ["abc", "def", "ghi"]
            returnJson $ object ["more" .= False, "results" .= V.fromList autJson]
        _ -> sendResponseStatus badRequest400 (systemError "Brak zapytania")
  where
    transform :: T.Text -> Value
    transform t = object ["id" .= t, "text" .= t]
