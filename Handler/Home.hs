module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
    baseLayout "Home" Nothing [whamlet|
    Home!
    |]

postHomeR :: Handler Html
postHomeR = do
    baseLayout "Home" Nothing [whamlet|

    |]
