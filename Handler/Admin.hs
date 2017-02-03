module Handler.Admin where

import Import

getAdminR :: Handler Html
getAdminR = do
  _ <- requireAdmin
  baseLayout "Admin" Nothing [whamlet|
<div .row #content>
  <div .medium-8 .columns>
    <h3>Welcome to the Admin page
    <ul>
      <li><a href="@{AdminCardR}">Create a new card
      <li><a href="@{AdminDeckR}">Create a new deck
|]


getAdminDeckR :: Handler Html
getAdminDeckR = do
  _ <- requireAdmin
  baseLayout "Admin - Create a Deck" Nothing [whamlet|
<div .row #content>
  <div .medium-8 .columns>
    <h3>Create a deck
|]

postAdminDeckR :: Handler Html
postAdminDeckR = do
  _ <- requireAdmin
  baseLayout "Admin - Create a Deck" Nothing [whamlet|
<div .row #content>
  <div .medium-8 .columns>
    <h3>Create a deck
|]


getAdminCardR :: Handler Html
getAdminCardR = do
  _ <- requireAdmin
  baseLayout "Admin - Create a Deck" Nothing [whamlet|
<div .row #content>
  <div .medium-8 .columns>
    <h3>Create a deck
|]

postAdminCardR :: Handler Html
postAdminCardR = do
  _ <- requireAdmin
  baseLayout "Admin - Create a Deck" Nothing [whamlet|
<div .row #content>
  <div .medium-8 .columns>
    <h3>Create a deck
|]
