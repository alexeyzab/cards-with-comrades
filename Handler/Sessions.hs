{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Sessions where

import Import.NoFoundation
import Control.Monad.Trans.Maybe
import Data.Time.Clock (addUTCTime)

type YesodLog site = (Yesod site)

userSess :: Text
userSess = "user_id"

rememberSess :: Text
rememberSess = "remember_me"

timestampSess :: Text
timestampSess = "time_created"

setUserSession :: (YesodLog site)
               => Key User
               -> Bool
               -> HandlerT site IO ()
setUserSession keyUser rememberMe = do
  setSession userSess (toPathPiece keyUser)
  if rememberMe
    then setSession rememberSess "True"
    else deleteSession rememberSess
  t <- liftIO getCurrentTime
  setSession timestampSess (toPathPiece (SessionTime t))
  return ()

newtype SessionTime =
    SessionTime UTCTime
     deriving (Eq, Read, Show)

instance PathPiece SessionTime where
    fromPathPiece = readMay . unpack
    toPathPiece = tshow

sessionTooOld
    :: (YesodLog site)
    => UTCTime -> HandlerT site IO Bool
sessionTooOld currentTime = do
    remember <- getSessionKey rememberSess
    timestamp <- getSessionKey timestampSess
    case timestamp >>= (fromPathPiece . decodeUtf8) of
        Nothing -> return True
        (Just (SessionTime t))
        -- no remember flag, so should only last 2 hours
         -> do
            let shortLife = 60 * 60 * 2
                -- shortLife = 5 -- for testing
                -- there was a remember flag, so we give it 1 month
                longLife = 60 * 60 * 24 * 30 * 1
                secondsToLast = maybe shortLife (const longLife) remember
                deadline = addUTCTime secondsToLast t
            -- if currentTime is greater than the
            -- session deadline, it's too old.
            return (currentTime > deadline)

sessionMiddleware
    :: (YesodLog site)
    => HandlerT site IO resp -> HandlerT site IO resp
sessionMiddleware handler = do
    t <- liftIO getCurrentTime
    tooOld <- sessionTooOld t
    if tooOld
        then deleteLoginData >> handler
        else handler

getSessionKey
    :: Text
    -> YesodLog site =>
       HandlerT site IO (Maybe ByteString)
getSessionKey k = do
    sess <- getSession
    return $ lookup k sess

getSessionUserK
    :: YesodLog site
    => HandlerT site IO (Maybe ByteString)
getSessionUserK = getSessionKey userSess

handleDumpSessionR
    :: YesodLog site
    => HandlerT site IO Text
handleDumpSessionR = tshow <$> getSession

deleteLoginData
    :: YesodLog site
    => HandlerT site IO ()
deleteLoginData = do
  deleteSession userSess
  deleteSession rememberSess
  deleteSession timestampSess

getUserKey
    :: (YesodLog site)
    => HandlerT site IO (Maybe (Key User))
getUserKey =
    runMaybeT $
    do userId <- MaybeT getSessionUserK
       userInt <- justZ $ fromPathPiece (decodeUtf8 userId)
       return (toSqlKey userInt)

getUser
    :: (YesodLog site, YesodPersist site, YesodPersistBackend site ~ SqlBackend)
    => HandlerT site IO (Maybe (Entity User))
getUser =
    runMaybeT $
    do userKey <- MaybeT $ getUserKey
       user <- MaybeT $ runDB $ get userKey
       return $ Entity userKey user

requireAdmin :: (YesodLog site, YesodPersist site, YesodPersistBackend site ~ SqlBackend)
             => HandlerT site IO (Entity User, Entity Admin)
requireAdmin = do
  maybeUser <- getUser
  case maybeUser of
    Nothing -> notAuthenticated
    (Just user) -> do
      maybeAdmin <- runDB $ fetchThingByField AdminAccount (entityKey user)
      case maybeAdmin of
        Nothing -> permissionDenied "You are not an administrator"
        (Just admin) -> return (user, admin)

