module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
    mbUser <- getUser
    baseLayout "Home" mbUser [whamlet|
    Home!
    |]

postHomeR :: Handler Html
postHomeR = do
    baseLayout "Home" Nothing [whamlet|

    |]
