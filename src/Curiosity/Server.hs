{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds
           , TypeOperators
#-} -- Language extensions needed for servant.
{-# LANGUAGE ConstraintKinds #-}
{- |
Module: Curiosity.Server
Description: Server root module, split up into public and private sub-modules.

-}
module Curiosity.Server
  (
    -- * Top level server types.
    App
  , ServerConf(..)
  , serverT
  , serve
  , run

    -- * Type-aliases for convenience
  , ServerSettings
  ) where

import qualified Commence.Multilogging         as ML
import qualified Commence.Runtime.Errors       as Errs
import qualified Commence.Runtime.Storage      as S
import           Control.Lens
import "exceptions" Control.Monad.Catch         ( MonadMask )
import qualified Crypto.JOSE.JWK               as JWK
import           Curiosity.Data                 ( HaskDb
                                                , readFullStmDbInHask
                                                )
import qualified Curiosity.Data.User           as User
import qualified Curiosity.Form.Login          as Login
import qualified Curiosity.Form.Signup         as Signup
import qualified Curiosity.Html.Errors         as Pages
import qualified Curiosity.Html.Homepage       as Pages
import qualified Curiosity.Html.LandingPage    as Pages
import qualified Curiosity.Html.Profile        as Pages
import qualified Curiosity.Runtime             as Rt
import qualified Curiosity.Server.Helpers      as H
import           Data.Aeson                     ( FromJSON
                                                , eitherDecode
                                                )
import qualified Data.ByteString.Lazy          as BS
import qualified Data.Text                     as T
import qualified Network.HTTP.Types            as HTTP
import qualified Network.Wai                   as Wai
import qualified Network.Wai.Handler.Warp      as Warp
import           Servant                 hiding ( serve )
import qualified Servant.Auth.Server           as SAuth
import qualified Servant.HTML.Blaze            as B
import qualified Servant.Server                as Server
import           Smart.Server.Page              ( PageEither )
import qualified Smart.Server.Page             as SS.P
import           System.FilePath                ( (</>) )
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Renderer.Utf8       ( renderMarkup )
import           WaiAppStatic.Storage.Filesystem
                                                ( defaultWebAppSettings )
import           WaiAppStatic.Storage.Filesystem.Extended
                                                ( hashFileIfExists
                                                , webAppLookup
                                                )
import           WaiAppStatic.Types             ( ss404Handler
                                                , ssLookupFile
                                                )


--------------------------------------------------------------------------------
-- | HTTP server config.
data ServerConf = ServerConf
  { _serverPort          :: Int
  , _serverStaticDir     :: FilePath
  , _serverDataDir       :: FilePath
  , _serverCookie        :: SAuth.CookieSettings
    -- ^ Settings for setting cookies as a server (for authentication etc.).
  , _serverMkJwtSettings :: JWK.JWK -> SAuth.JWTSettings
    -- ^ JWK settings to use, depending on the key employed.
  }


--------------------------------------------------------------------------------
-- brittany-disable-next-binding
-- | This is the main Servant API definition for Curiosity.
type App = H.UserAuthentication :> Get '[B.HTML] (PageEither
               Pages.LandingPage
               Pages.WelcomePage
             )
             :<|> "forms" :> "login" :> Get '[B.HTML] Login.Page
             :<|> "forms" :> "signup" :> Get '[B.HTML] Signup.Page
             :<|> "forms" :> "profile" :> Get '[B.HTML] Pages.ProfilePage

             :<|> "views" :> "profile"
                  :> Capture "filename" FilePath
                  :> Get '[B.HTML] Pages.ProfileView

             :<|> "messages" :> "signup" :> Get '[B.HTML] Signup.SignupResultPage

             :<|> "state" :> Get '[B.HTML] Login.ResultPage -- TODO Proper type.
             :<|> "state.json" :> Get '[JSON] (HaskDb Rt.Runtime)

             :<|> "echo" :> "login"
                  :> ReqBody '[FormUrlEncoded] User.Credentials
                  :> Post '[B.HTML] Login.ResultPage
             :<|> "echo" :> "signup"
                  :> ReqBody '[FormUrlEncoded] User.Signup
                  :> Post '[B.HTML] Signup.ResultPage

             :<|> "login" :> Get '[B.HTML] Login.Page
             :<|> "signup" :> Get '[B.HTML] Signup.Page

             :<|> Public
             :<|> Private
             :<|> "data" :> Raw
             :<|> Raw -- Catchall for static files (documentation)
                      -- and for a custom 404

-- | This is the main Servant server definition, corresponding to @App@.
serverT
  :: forall m
   . ServerC m
  => ServerConf
  -> SAuth.JWTSettings
  -> FilePath
  -> FilePath
  -> ServerT App m
serverT conf jwtS root dataDir =
  showLandingPage
    :<|> documentLoginPage
    :<|> documentSignupPage
    :<|> documentEditProfilePage
    :<|> documentProfilePage dataDir
    :<|> messageSignupSuccess
    :<|> showState
    :<|> showStateAsJson
    :<|> echoLogin
    :<|> echoSignup
    :<|> showLoginPage
    :<|> showSignupPage
    :<|> publicT conf jwtS
    :<|> privateT
    :<|> serveData dataDir
    :<|> serveDocumentation root


--------------------------------------------------------------------------------
-- | Minimal set of constraints needed on some monad @m@ to be satisfied to be
-- able to run a server.
type ServerC m
  = ( MonadMask m
    , ML.MonadAppNameLogMulti m
    , S.DBStorage m User.UserProfile
    , MonadReader Rt.Runtime m
    , MonadIO m
    )


--------------------------------------------------------------------------------
type ServerSettings = '[SAuth.CookieSettings , SAuth.JWTSettings]

-- | This is the main function of this module. It runs a Warp server, serving
-- our @App@ API definition.
run
  :: forall m
   . MonadIO m
  => ServerConf
  -> Rt.Runtime -- ^ Runtime to use for running the server.
  -> m ()
run conf@ServerConf {..} runtime@Rt.Runtime {..} = liftIO $ do
  jwk <- SAuth.generateKey
  let jwtSettings = _serverMkJwtSettings jwk
  Warp.run _serverPort $ waiApp jwtSettings
 where
  waiApp jwtS = serve @Rt.AppM (Rt.appMHandlerNatTrans runtime)
                               conf
                               (ctx jwtS)
                               jwtS
                               _serverStaticDir
                               _serverDataDir
  ctx jwtS = _serverCookie Server.:. jwtS Server.:. Server.EmptyContext

-- | Turn our @serverT@ definition into a Wai application, suitable for
-- Warp.run.
serve
  :: forall m
   . ServerC m
  => (forall x . m x -> Handler x) -- ^ Natural transformation to transform an
                                   -- arbitrary @m@ to a Servant @Handler@
  -> ServerConf
  -> Server.Context ServerSettings
  -> SAuth.JWTSettings
  -> FilePath
  -> FilePath
  -> Wai.Application
serve handlerNatTrans conf ctx jwtS root dataDir =
  Servant.serveWithContext appProxy ctx
    $ hoistServerWithContext appProxy settingsProxy handlerNatTrans
    $ serverT conf jwtS root dataDir
 where
  appProxy      = Proxy @App
  settingsProxy = Proxy @ServerSettings


--------------------------------------------------------------------------------
-- | Show the landing page when the user is not logged in, or the welcome page
-- when the user is logged in.
showLandingPage
  :: ServerC m
  => SAuth.AuthResult User.UserId
  -> m (PageEither Pages.LandingPage Pages.WelcomePage)
     -- We don't use SS.P.Public Void, nor SS.P.Public 'Authd UserProfile
     -- to not have the automatic heading.
showLandingPage authResult = withMaybeUser
  authResult
  (\_ -> pure $ SS.P.PageL Pages.LandingPage)
  (\userProfile -> pure $ SS.P.PageR Pages.WelcomePage)


--------------------------------------------------------------------------------
showSignupPage :: ServerC m => m Signup.Page
showSignupPage = pure $ Signup.Page "/a/signup"

documentSignupPage :: ServerC m => m Signup.Page
documentSignupPage = pure $ Signup.Page "/echo/signup"

messageSignupSuccess :: ServerC m => m Signup.SignupResultPage
messageSignupSuccess = pure Signup.SignupSuccess

echoSignup :: ServerC m => User.Signup -> m Signup.ResultPage
echoSignup input = pure $ Signup.Success $ show input


--------------------------------------------------------------------------------
showLoginPage :: ServerC m => m Login.Page
showLoginPage = pure $ Login.Page "/a/login"

documentLoginPage :: ServerC m => m Login.Page
documentLoginPage = pure $ Login.Page "/echo/login"

echoLogin :: ServerC m => User.Credentials -> m Login.ResultPage
echoLogin input = pure $ Login.Success $ show input


--------------------------------------------------------------------------------
-- brittany-disable-next-binding
-- | A publicly available login page.
type Public =    "a" :> "signup"
                 :> ReqBody '[FormUrlEncoded] User.Signup
                 :> Post '[B.HTML] Signup.SignupResultPage
            :<|> "a" :> "login"
                 :> ReqBody '[FormUrlEncoded] User.Credentials
                 :> Verb 'POST 303 '[JSON] ( Headers H.PostAuthHeaders
                                              NoContent
                                            )

publicT
  :: forall m
   . ServerC m
  => ServerConf
  -> SAuth.JWTSettings
  -> ServerT Public m
publicT conf jwtS = handleSignup :<|> handleLogin conf jwtS

handleSignup
  :: forall m . ServerC m => User.Signup -> m Signup.SignupResultPage
handleSignup input@User.Signup {..} =
  ML.localEnv (<> "HTTP" <> "Signup")
    $   do
          ML.info $ "Signing up new user: " <> show username <> "..."
          -- TODO Generate deterministically within STM.
          newId <- User.genRandomUserId 10
          Rt.withRuntimeAtomically Rt.createUser (newId, input)
    >>= \case
          Right uid -> do
            ML.info
              $  "User created: "
              <> show uid
              <> ". Sending success result."
            pure Signup.SignupSuccess
          Left err -> do
            ML.info
              $  "Failed to create a user. Sending failure result: "
              <> show err
            Errs.throwError' err

handleLogin
  :: forall m
   . ServerC m
  => ServerConf
  -> SAuth.JWTSettings
  -> User.Credentials
  -> m (Headers H.PostAuthHeaders NoContent)
handleLogin conf jwtSettings input =
  ML.localEnv (<> "HTTP" <> "Login")
    $   do
          ML.info
            $  "Logging in user: "
            <> show (User._userCredsName input)
            <> "..."
          Rt.withRuntimeAtomically Rt.checkCredentials input
    >>= \case
          Right u -> do
            ML.info "Found user, applying authentication cookies..."
            Rt.Conf {..}  <- asks Rt._rConf
            -- TODO I think jwtSettings could be retrieved with
            -- Servant.Server.getContetEntry. This would avoid threading
            -- jwtSettings evereywhere.
            mApplyCookies <- liftIO $ SAuth.acceptLogin
              (_serverCookie conf)
              jwtSettings
              (u ^. User.userProfileId)
            case mApplyCookies of
              Nothing -> do
                -- From a quick look at Servant, it seems the error would be a
                -- JSON-encoding failure of the generated JWT.
                let err = ServerErr "Couldn't apply cookies."
                ML.error
                  $  "Applying cookies failed. Sending failure result: "
                  <> show err
                Errs.throwError' err
              Just applyCookies -> do
                ML.info "Cookies applied. Sending success result."
                pure . addHeader @"Location" "/" $ applyCookies NoContent
          Left err -> do
            ML.info
              $  "Incorrect username or password. Sending failure result: "
              <> show err
            Errs.throwError' err


--------------------------------------------------------------------------------
-- brittany-disable-next-binding
-- | The private API with authentication.
type Private = H.UserAuthentication :> (
                   "settings" :> "profile"
                   :> Get '[B.HTML] Pages.ProfileView
             :<|>  "settings" :> "profile.json"
                   :> Get '[JSON] User.UserProfile
             :<|>  "settings" :> "profile" :> "edit"
                   :> Get '[B.HTML] Pages.ProfilePage
             :<|>  "a" :>"set-user-profile"
                   :> ReqBody '[FormUrlEncoded] User.Update
                   :> H.PostUserPage Pages.ProfileSaveConfirmPage
  )

privateT :: forall m . ServerC m => ServerT Private m
privateT authResult =
  (withUser authResult showProfilePage)
    :<|> (withUser authResult showProfileAsJson)
    :<|> (withUser authResult showEditProfilePage)
    :<|> (withUser authResult . handleUserUpdate)

showProfilePage profile = pure $ Pages.ProfileView profile

showProfileAsJson
  :: forall m . ServerC m => User.UserProfile -> m User.UserProfile
showProfileAsJson = pure

showEditProfilePage profile =
  pure $ Pages.ProfilePage profile "/a/set-user-profile"

handleUserUpdate
  :: forall m
   . ServerC m
  => User.Update
  -> User.UserProfile
  -> m
       ( SS.P.Page
           'SS.P.Authd
           User.UserProfile
           Pages.ProfileSaveConfirmPage
       )
handleUserUpdate User.Update {..} profile = case _editPassword of
  Just newPass ->
    let updatedProfile =
          profile
            &  User.userProfileCreds
            .  User.userCredsPassword
            %~ (`fromMaybe` _editPassword)
    in  S.dbUpdate (User.UserPasswordUpdate (S.dbId profile) newPass)
          <&> headMay
          <&> SS.P.AuthdPage updatedProfile
          .   \case
                Nothing -> Pages.ProfileSaveFailure
                  $ Just "Empty list of affected User IDs on update."
                Just{} -> Pages.ProfileSaveSuccess
  Nothing -> pure . SS.P.AuthdPage profile . Pages.ProfileSaveFailure $ Just
    "Nothing to update."

documentEditProfilePage :: ServerC m => m Pages.ProfilePage
documentEditProfilePage = do
  profile <- readJson "data/alice.json"
  pure $ Pages.ProfilePage profile "/echo/profile"

-- TODO Validate the filename (e.g. this can't be a path going up).
documentProfilePage :: ServerC m => FilePath -> FilePath -> m Pages.ProfileView
documentProfilePage dataDir filename = do
  profile <- readJson $ dataDir </> filename
  pure $ Pages.ProfileView profile


--------------------------------------------------------------------------------
-- TODO When given a wrong path, e.g, in documentProfilePage above, the
-- returned HTML is not what I expect, nor the error is logged to our log files.
readJson :: (ServerC m, FromJSON a) => FilePath -> m a
readJson path = do
  content <- liftIO $ BS.readFile path
  let ma = eitherDecode content
  case ma of
    Right a -> pure a
    -- TODO Be more specific, e.g. the error can also be malformed JSON.
    Left  _ -> Errs.throwError' $ Rt.FileDoesntExistErr path


--------------------------------------------------------------------------------
-- | Run a handler, ensuring a user profile can be extracted from the
-- authentication result, or throw an error.
withUser
  :: forall m a
   . ServerC m
  => SAuth.AuthResult User.UserId
  -> (User.UserProfile -> m a)
  -> m a
withUser authResult f = withMaybeUser authResult (authFailedErr . show) f
 where
  authFailedErr = Errs.throwError' . User.UserNotFound . mappend
    "Authentication failed, please login again. Error: "

-- | Run either a handler expecting a user profile, or a normal handler,
-- depending on if a user profile can be extracted from the authentication
-- result or not.
withMaybeUser
  :: forall m a
   . ServerC m
  => SAuth.AuthResult User.UserId
  -> (SAuth.AuthResult User.UserId -> m a)
  -> (User.UserProfile -> m a)
  -> m a
withMaybeUser authResult a f = case authResult of
  SAuth.Authenticated userId ->
    S.dbSelect (User.SelectUserById userId) <&> headMay >>= \case
      Nothing -> do
        ML.warning
          "Cookie-based authentication succeeded, but the user ID is not found."
        authFailedErr $ "No user found with ID " <> show userId
      Just userProfile -> f userProfile
  authFailed -> a authFailed
  where authFailedErr = Errs.throwError' . User.UserNotFound


--------------------------------------------------------------------------------
showState :: ServerC m => m Login.ResultPage
showState = do
  stmDb <- asks Rt._rDb
  db    <- readFullStmDbInHask stmDb
  pure . Login.Success $ show db

-- TODO The passwords are displayed in clear. Would be great to have the option
-- to hide/show them.
showStateAsJson :: ServerC m => m (HaskDb Rt.Runtime)
showStateAsJson = do
  stmDb <- asks Rt._rDb
  db    <- readFullStmDbInHask stmDb
  pure db


--------------------------------------------------------------------------------
-- | Serve the static files for the documentation. This also provides a custom
-- 404 fallback.
serveDocumentation root = serveDirectoryWith settings
 where
  settings = (defaultWebAppSettings root)
    { ss404Handler = Just custom404
    , ssLookupFile = webAppLookup hashFileIfExists root
    }

custom404 :: Application
custom404 _request sendResponse = sendResponse $ Wai.responseLBS
  HTTP.status404
  [("Content-Type", "text/html; charset=UTF-8")]
  (renderMarkup $ H.toMarkup Pages.NotFoundPage)


--------------------------------------------------------------------------------
-- | Serve example data as JSON files.
serveData path = serveDirectoryWith settings
 where
  settings = (defaultWebAppSettings path) { ss404Handler = Just custom404 }


--------------------------------------------------------------------------------
newtype ServerErr = ServerErr Text
  deriving Show

instance Errs.IsRuntimeErr ServerErr where
  errCode ServerErr{} = "ERR.INTERNAL"
  httpStatus ServerErr{} = HTTP.internalServerError500
  userMessage = Just . \case
    ServerErr msg -> T.unwords ["Internal server error: ", msg]
