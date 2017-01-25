module Helpers.Views where

import Import.NoFoundation

import AppType
import Handler.Sessions

baseTemplate :: PageContent url -> Handler Html
baseTemplate pc = do
  [whamlet|
<!DOCTYPE html>
<html lang="en">
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        <meta name="description" content="Cards with Comrades" />

        <meta name="viewport" content="width=device-width, initial-scale=1">
        <title>cardswithcomrades.com - #{pageTitle pc}

        ^{pageHead pc}

    <body>
        ^{pageBody pc}

        <footer id="page-footer">
            <div class="container">
                <div class="row">
                    <div class="col-sm-12 text-center">
                        <ul class="list-unstyled">
                            <li>
                                <a href="/about">about
                <div class="row">
                    <div class="col-sm-12 text-center">
                      <p .text-muted>
                        #{appCopyright $ appSettings master}
0
|]

baseLayout :: Html -> Maybe (Entity User) -> WidgetT site IO () -> HandlerT site IO Html
baseLayout title user content = do
  defaultLayout $ do
    setTitle title
    [whamlet|
    <div .container>
      hi
    ^{content}
    |]
