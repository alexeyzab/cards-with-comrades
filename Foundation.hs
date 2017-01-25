module Foundation where

import Import.NoFoundation

import AppType
import Routes

import qualified Data.CaseInsensitive as CI

import Helpers.Views
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)

import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe

import Handler.Sessions

htmlOnly :: (MonadHandler m) => m Html -> m TypedContent
htmlOnly = selectRep . provideRep

instance Yesod App where
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    yesodMiddleware = sessionMiddleware . defaultYesodMiddleware

    defaultLayout widget = do
        master <- getYesod
        mcurrentRoute <- getCurrentRoute
        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            $(widgetFile "default-layout")
        withUrlRenderer baseTemplate

    errorHandler NotFound =
      htmlOnly $ defaultLayout $ do
        setTitle "Not found!"
        $(widgetFile "errors/404")

    errorHandler (InternalError err) =
      htmlOnly $ defaultLayout $ do
        setTitle "Our bad!"
        $(widgetFile "errors/500")

    errorHandler (InvalidArgs _) =
      htmlOnly $ defaultLayout $ do
        setTitle "Invalid request"
        $(widgetFile "errors/400")      

    errorHandler NotAuthenticated =
      htmlOnly $ defaultLayout $ do
        setTitle "Not authenticated"
        $(widgetFile "errors/401")      

    errorHandler (PermissionDenied _) =
      htmlOnly $ defaultLayout $ do
        setTitle "Permission denied"
        $(widgetFile "errors/403")      

    errorHandler (BadMethod _) =
      htmlOnly $ defaultLayout $ do
        setTitle "Bad method for request"
        $(widgetFile "errors/400")      

    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        genFileName lbs = "autogen-" ++ base64md5 lbs

    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger
