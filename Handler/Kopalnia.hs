module Handler.Kopalnia where

import Import
-- import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, withSmallInput)

getKopalniaMainR :: Handler Html
getKopalniaMainR = do
    defaultLayout $ do
        setTitle "Polska Bibliografia Wiedzy o Komiksie - Zeszyty Komiksowe"
        $(widgetFile "kopalnia-main")
