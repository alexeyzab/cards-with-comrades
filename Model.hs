{-# LANGUAGE FlexibleInstances #-}

module Model
  ( module Import
  , module Model
  ) where

import ClassyPrelude.Yesod hiding ((==.), hash, on)
import Control.Monad.Logger hiding (LoggingT, runLoggingT)
import Data.Maybe (listToMaybe)
-- import Data.UUID
import Database.Esqueleto
import Database.Persist.Postgresql (ConnectionString, withPostgresqlPool)
-- import Database.Persist.Quasi
-- import Database.Persist.Sql

import Model.BCrypt as Import
import Model.Instances as Import ()
import Model.Types as Import

-- data Card =
--     Adlib AdlibCard
--   | Answer AnswerCard

-- ZeroBlankAdlibs are questions
-- data AdlibCard =
--     ZeroBlankAdlib !Text
--   | OneBlankAdlib  !Text !Text
--   | TwoBlankAdlib  !Text !Text !Text

-- newtype AnswerCard = AnswerCard Text

-- True ["Sonic the Hedgehog"]
-- False ["Wow he really", "the white house"]

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json sql=users
  email Text
  UniqueUserEmail email
  deriving Eq Show

Password sql=passwords
  hash BCrypt
  user UserId
  UniquePasswordUser user
  deriving Eq Show

Admin sql=admins
  account UserId
  UniqueAdminUser account
  deriving Eq Show

Card json sql=cards
  isAnswer Bool
  text1 Text
  text2 Text Maybe
  text3 Text Maybe
  text4 Text Maybe
  text5 Text Maybe
  text6 Text Maybe
  text7 Text Maybe
  text8 Text Maybe
  text9 Text Maybe
  deriving Eq Show

Deck json sql=decks
  title Text
  deriving Eq Show

DeckCard json sql=deckcards
  dcCard CardId
  dcDeck DeckId
  UniqueDeckCard dcCard dcDeck
  deriving Eq Show

Tag json sql=tags
  name Text
  deriving Eq Show

DeckTag json sql=decktags
  dtTag TagId
  dtDeck DeckId
  UniqueDeckTag dtTag dtDeck
  deriving Eq Show
|]

getUserPassword :: Text -> DB (Maybe (Entity User, Entity Password))
getUserPassword email = fmap listToMaybe $
  select $
  from $ \(user `InnerJoin` pass) -> do
  on (user ^. UserId ==. pass ^. PasswordUser)
  where_ (user ^. UserEmail ==. val email)
  return (user, pass)

getUserEntity :: Text -> DB (Maybe (Entity User))
getUserEntity email = fmap listToMaybe $
  select $
  from $ \user -> do
  where_ (user ^. UserEmail ==. val email)
  return user

createUser :: Text -> Text -> DB (Entity User)
createUser email pass = do
  let newUser = User email
  userId <- insert $ newUser
  hash <- liftIO $ hashPassword pass
  _ <- insert $ Password hash userId
  return (Entity userId newUser)

createAdmin :: Key User -> DB (Entity Admin)
createAdmin userKey = do
  let newAdmin = Admin userKey
  adminKey <- insert $ newAdmin
  return (Entity adminKey newAdmin)

createCard :: Card -> DB (Entity Card)
createCard newCard = do
  cardKey <- insert $ newCard
  return (Entity cardKey newCard)

dumpMigration :: DB ()
dumpMigration = printMigration migrateAll

runMigrations :: DB ()
runMigrations = runMigration migrateAll

devConn :: ConnectionString
devConn =
  "dbname=cards-with-comrades-dev host=localhost user=postgres password=password port=5432"

runDevDB :: DB a -> IO a
runDevDB a =
  runNoLoggingT $
    withPostgresqlPool devConn 3
      $ \pool -> liftIO $ runSqlPersistMPool a pool

runDevDBV :: DB a -> IO a
runDevDBV a =
  runStdoutLoggingT $
    withPostgresqlPool devConn 3
      $ \pool -> liftIO $ runSqlPersistMPool a pool
