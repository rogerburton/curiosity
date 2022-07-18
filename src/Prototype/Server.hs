{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds
           , TypeOperators
#-} -- Language extensions needed for servant.
{- |
Module: Prototype.Server
Description: Server root module, split up into public and private sub-modules.

-}
module Prototype.Server
  (
    -- * Top level server types.
    App
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
import qualified Network.HTTP.Types            as HTTP
import qualified Network.Wai                   as Wai
import qualified Network.Wai.Handler.Warp      as Warp
import qualified Prototype.Data.User           as User
import qualified Prototype.Form.Login          as Login
import qualified Prototype.Form.Signup         as Signup
import qualified Prototype.Runtime             as Rt
import qualified Prototype.Server.Private      as Priv
import qualified Prototype.Server.Private.Helpers
                                               as H
import qualified Prototype.Server.Private.Pages
                                               as Pages
import qualified Prototype.Server.Public       as Pub
import qualified Prototype.Server.Public.Pages
                                               as Pages
import           Servant                 hiding ( serve )
import qualified Servant.Auth.Server           as SAuth
import qualified Servant.HTML.Blaze            as B
import qualified Servant.Server                as Server
import           Smart.Server.Page              ( PageEither )
import qualified Smart.Server.Page             as SS.P
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Renderer.Utf8       ( renderMarkup )
import           WaiAppStatic.Storage.Filesystem
                                                ( defaultWebAppSettings
                                                )
import WaiAppStatic.Storage.Filesystem.Extended ( hashFileIfExists, webAppLookup )
import           WaiAppStatic.Types             ( ss404Handler
                                                , ssLookupFile
                                                )


--------------------------------------------------------------------------------
-- brittany-disable-next-binding
-- | This is the main Servant API definition for Curiosity.
type App = H.UserAuthentication :> Get '[B.HTML] (PageEither
               Pages.LandingPage
               Pages.WelcomePage
             )
             :<|> "forms" :> "login" :> Get '[B.HTML] Login.Page
             :<|> "forms" :> "signup" :> Get '[B.HTML] Signup.Page
             -- TODO Add the user profile update form.
             -- TODO Add the user profile view.

             :<|> "messages" :> "signup" :> Get '[B.HTML] Signup.SignupResultPage

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
             :<|> Raw -- Catchall for static files (documentation)
                      -- and for a custom 404

-- | This is the main Servant server definition, corresponding to @App@.
serverT :: forall m . Pub.PublicServerC m => FilePath -> ServerT App m
serverT root =
  showLandingPage
    :<|> documentLoginPage
    :<|> documentSignupPage
    :<|> messageSignupSuccess
    :<|> echoLogin
    :<|> echoSignup
    :<|> showLoginPage
    :<|> showSignupPage
    :<|> publicT
    :<|> privateT
    :<|> serveDocumentation root


--------------------------------------------------------------------------------
type ServerSettings = '[SAuth.CookieSettings , SAuth.JWTSettings]

-- | This is the main function of this module. It runs a Warp server, serving
-- our @App@ API definition.
run
  :: forall m
   . MonadIO m
  => Rt.Runtime -- ^ Runtime to use for running the server.
  -> m ()
run runtime@Rt.Runtime {..} = liftIO $ Warp.run port waiApp
 where
  Rt.ServerConf port root = runtime ^. Rt.rConf . Rt.confServer
  waiApp = serve @Rt.ExeAppM (Rt.exampleAppMHandlerNatTrans runtime) ctx root
  ctx =
    _rConf
      ^.        Rt.confCookie
      Server.:. _rJwtSettings
      Server.:. Server.EmptyContext

-- | Turn our @serverT@ definition into a Wai application, suitable for
-- Warp.run.
serve
  :: forall m
   . Pub.PublicServerC m
  => (forall x . m x -> Handler x) -- ^ Natural transformation to transform an
                                   -- arbitrary @m@ to a Servant @Handler@
  -> Server.Context ServerSettings
  -> FilePath
  -> Wai.Application
serve handlerNatTrans ctx root =
  Servant.serveWithContext appProxy ctx
    $ hoistServerWithContext appProxy settingsProxy handlerNatTrans
    $ serverT root
 where
  appProxy      = Proxy @App
  settingsProxy = Proxy @ServerSettings


--------------------------------------------------------------------------------
-- | Show the landing page when the user is not logged in, or the welcome page
-- when the user is logged in.
showLandingPage
  :: Pub.PublicServerC m
  => SAuth.AuthResult User.UserId
  -> m (PageEither
           -- We don't use SS.P.Public Void, nor SS.P.Public 'Authd UserProfile
           -- to not have the automatic heading.
                   Pages.LandingPage Pages.WelcomePage)
showLandingPage = \case
  SAuth.Authenticated userId ->
    S.dbSelect (User.SelectUserById userId) <&> headMay >>= \case
      Nothing -> do
        ML.warning
          "Cookie-based authentication succeeded, but the user ID is not found."
        authFailedErr $ "No user found with ID " <> show userId
      Just userProfile -> pure $ SS.P.PageR Pages.WelcomePage
  _ -> pure $ SS.P.PageL Pages.LandingPage
  where authFailedErr = Errs.throwError' . User.UserNotFound


--------------------------------------------------------------------------------
showSignupPage :: Pub.PublicServerC m => m Signup.Page
showSignupPage = pure $ Signup.Page "/a/signup"

documentSignupPage :: Pub.PublicServerC m => m Signup.Page
documentSignupPage = pure $ Signup.Page "/echo/signup"

messageSignupSuccess :: Pub.PublicServerC m => m Signup.SignupResultPage
messageSignupSuccess = pure Signup.SignupSuccess

echoSignup :: Pub.PublicServerC m => User.Signup -> m Signup.ResultPage
echoSignup input = pure $ Signup.Success $ show input


--------------------------------------------------------------------------------
showLoginPage :: Pub.PublicServerC m => m Login.Page
showLoginPage = pure $ Login.Page "/a/login"

documentLoginPage :: Pub.PublicServerC m => m Login.Page
documentLoginPage = pure $ Login.Page "/echo/login"

echoLogin :: Pub.PublicServerC m => User.Credentials -> m Login.ResultPage
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

publicT :: forall m . Pub.PublicServerC m => ServerT Public m
publicT = handleSignup :<|> handleLogin

handleSignup User.Signup {..} = env $ do
  ML.info $ "Signing up new user: " <> show username <> "..."
  ids <- S.dbUpdate $ User.UserCreateGeneratingUserId username password email
  case headMay ids of
    Just uid -> do
      ML.info $ "User created: " <> show uid <> ". Sending success result."
      pure Signup.SignupSuccess
    -- TODO Failure to create a user re-using an existing username doesn't
    -- trigger the Nothing case.
    Nothing -> do
      -- TODO This should not be a 200 OK result.
      ML.info $ "Failed to create a user. Sending failure result."
      pure $ Signup.SignupFailed "Failed to create users."
  where env = ML.localEnv (<> "HTTP" <> "Signup")

handleLogin User.Credentials {..} =
  env $ findMatchingUsers <&> headMay >>= \case
    Just u -> do
      ML.info "Found user, generating authentication cookies..."
      jwtSettings   <- asks Rt._rJwtSettings
      Rt.Conf {..}  <- asks Rt._rConf
      mApplyCookies <- liftIO
        $ SAuth.acceptLogin _confCookie jwtSettings (u ^. User.userProfileId)
      ML.info "Applying cookies..."
      case mApplyCookies of
        Nothing -> do
          -- TODO What can cause a failure here ?
          ML.warning "Applying cookies failed. Sending failure result."
          unauthdErr _userCredsName
        Just applyCookies -> do
          ML.info "Cookies applied. Sending success result."
          pure . addHeader @"Location" "/" $ applyCookies NoContent
    -- TODO This is wrong: if UserLoginWithUserName doesn't find a user, it
    -- throws an error instead of returning a Nothing. So either change its
    -- logic to return a Nothing or an empty list, or catch the exception.
    Nothing -> do
      ML.info "User not found. Sending Failure result."
      unauthdErr _userCredsName
 where
  env               = ML.localEnv (<> "HTTP" <> "Login")
  findMatchingUsers = do
    ML.info $ "Logging in user: " <> show _userCredsName <> "..."
    S.dbSelect $ User.UserLoginWithUserName _userCredsName _userCredsPassword
  unauthdErr =
    Errs.throwError'
      . User.IncorrectPassword
      . mappend "User login failed: "
      . show


--------------------------------------------------------------------------------
-- brittany-disable-next-binding
-- | The private API with authentication.
type Private = H.UserAuthentication :> (
                   "settings" :> "profile"
                   :> Get '[B.HTML] Pages.ProfileView
             :<|>  "settings" :> "profile" :> "edit"
                   :> Get '[B.HTML] Pages.ProfilePage
             :<|>  "a" :>"set-user-profile"
                   :> ReqBody '[FormUrlEncoded] User.Update
                   :> H.PostUserPage Pages.ProfileSaveConfirmPage
  )

privateT :: forall m . Priv.PrivateServerC m => ServerT Private m
privateT authResult =
  (withUser authResult showProfilePage)
    :<|> (withUser authResult showEditProfilePage)
    :<|> (withUser authResult . handleUserUpdate)

showProfilePage profile = pure $ Pages.ProfileView profile

showEditProfilePage profile =
  pure $ Pages.ProfilePage profile "/a/set-user-profile"

handleUserUpdate
  :: forall m
   . Priv.PrivateServerC m
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

-- | Run a handler, ensuring a user profile can be extracted from the
-- authentication result, or throw an error.
withUser
  :: forall m a
   . Priv.PrivateServerC m
  => SAuth.AuthResult User.UserId
  -> (User.UserProfile -> m a)
  -> m a
withUser authResult f = case authResult of
  SAuth.Authenticated userId ->
    S.dbSelect (User.SelectUserById userId) <&> headMay >>= \case
      Nothing -> authFailedErr $ "No user found by ID = " <> show userId
      Just userProfile -> f userProfile
  authFailed -> authFailedErr $ show authFailed
 where
  authFailedErr = Errs.throwError' . User.UserNotFound . mappend
    "Authentication failed, please login again. Error: "


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
