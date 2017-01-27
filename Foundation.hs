module Foundation where

import Import.NoFoundation

import qualified Data.CaseInsensitive as CI

import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)

import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe

import Handler.Sessions

data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

mkYesodData "App" [parseRoutes|
/static StaticR Static appStatic

/favicon.ico FaviconR GET
/robots.txt RobotsR GET

/           HomeR    GET POST
/login      LoginR   GET POST
/signup     SignupR  GET POST
/signout    SignoutR GET
|]

htmlOnly :: (MonadHandler m) => m Html -> m TypedContent
htmlOnly = selectRep . provideRep

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

navLayout :: Maybe (Entity User) -> Widget
navLayout user =
  [whamlet|
<div class="top-bar">
  <div class="top-bar-left">
    <ul class="menu">
      <li .menu-logo>
        <a href="@{HomeR}" .plain>Cards With Comrades
  <div class="top-bar-right">
    <ul class="menu">
      $maybe _ <- user
        <li>
          <a href="@{SignoutR}">Signout
      $nothing
        <li>
          <a href="@{LoginR}">Login
        <li>
          <a href="@{SignupR}">Signup
|]

baseLayout :: Html -> Maybe (Entity User) -> WidgetT App IO () -> Handler Html
baseLayout title user content = do
  defaultLayout $ do
    setTitle title
    [whamlet|
^{navLayout user}
^{content}
|]

errorFragment :: Text -> Widget
errorFragment t =
  [whamlet|
<div .row #content>
  <div .large-8 .columns>
    <h1>#{t}
|]

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
      pc <- widgetToPageContent [whamlet|^{widget}|]
      withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    errorHandler NotFound = do
      user <- getUser
      htmlOnly $ baseLayout "Not found!" user $ errorFragment "404"

    errorHandler (InternalError err) = do
      user <- getUser
      htmlOnly $ baseLayout "Our bad!" user $ errorFragment "500"

    errorHandler (InvalidArgs _) = do
      user <- getUser
      htmlOnly $ baseLayout "Invalid request" user $ errorFragment "400"

    errorHandler NotAuthenticated = do
      user <- getUser
      htmlOnly $ baseLayout "Not authenticated" user $ errorFragment "401"

    errorHandler (PermissionDenied _) = do
      user <- getUser
      htmlOnly $ baseLayout "Permission denied" user $ errorFragment "403"

    errorHandler (BadMethod _) = do
      user <- getUser
      htmlOnly $ baseLayout "Bad method for request" user $ errorFragment "400"

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
