{-# LANGUAGE FlexibleInstances #-}

module Model
  ( module Import
  , module Model
  ) where

import ClassyPrelude.Yesod hiding ((==.), on)
import Control.Monad.Logger hiding (LoggingT, runLoggingT)
import Data.Maybe (listToMaybe)
import Data.UUID
import Database.Esqueleto
import Database.Persist.Postgresql (ConnectionString, withPostgresqlPool)
import Database.Persist.Quasi
-- import Database.Persist.Sql

import Model.BCrypt as Import
import Model.Instances as Import ()
import Model.Types as Import

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json sql=users
    email Text
    UniqueUserEmail email
    deriving Show

Password sql=passwords
    hash BCrypt
    user UserId
    UniquePasswordUser user
    deriving Show
|]

getUserPassword :: Text -> DB (Maybe (Entity User, Entity Password))
getUserPassword email = fmap listToMaybe $
  select $
  from $ \(user `InnerJoin` pass) -> do
  on (user ^. UserId ==. pass ^. PasswordUser)
  where_ (user ^. UserEmail ==. val email)
  return (user, pass)

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
