{-# LANGUAGE FlexibleInstances #-}

module Model
  ( module Import
  , module Model
  ) where

import ClassyPrelude.Yesod
import Control.Monad.Logger hiding (LoggingT, runLoggingT)
import Data.UUID
import Database.Persist.Postgresql
import Database.Persist.Quasi
import Database.Persist.Sql

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
