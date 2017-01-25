module Helpers.Views where

import Foundation
import Import.NoFoundation

import Handler.Sessions

baseLayout :: Html -> Maybe (Entity User) -> WidgetT App IO () -> Handler Html
baseLayout title user content = do
  defaultLayout $ do
    setTitle title
    [whamlet|
    <div .container>
      hi
    ^{content}
    |]
