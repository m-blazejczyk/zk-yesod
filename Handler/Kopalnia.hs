module Handler.Kopalnia where

import Import
-- import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, withSmallInput)

getKopalniaR :: Handler Html
getKopalniaR = do
    defaultLayout $ do
        setTitle "Zeszyty Komiksowe - Bibliografia wiedzy o komiksie"
        $(widgetFile "kopalnia-main")
