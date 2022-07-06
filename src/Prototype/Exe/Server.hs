{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds
           , TypeOperators
#-} -- Language extensions needed for servant.
{- |
Module: Prototype.Exe.Server
Description: Server root module, split up into public and private sub-modules.

-}
module Prototype.Exe.Server
  (
    -- * Top level server types.
    Exe
  , exampleT
  , exampleApplication
  , runExeServer
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
import qualified Prototype.Exe.Data.User       as User
import qualified Prototype.Exe.Form.Login      as Login
import qualified Prototype.Exe.Form.Signup     as Signup
import qualified Prototype.Exe.Runtime         as Rt
import qualified Prototype.Exe.Server.Private  as Priv
import qualified Prototype.Exe.Server.Private.Auth
                                               as Auth
import qualified Prototype.Exe.Server.Private.Pages
                                               as Pages
import qualified Prototype.Exe.Server.Public   as Pub
import qualified Prototype.Exe.Server.Public.Pages
                                               as Pages
import           Servant
import qualified Servant.Auth.Server           as SAuth
import qualified Servant.HTML.Blaze            as B
import qualified Servant.Server                as Server
import           Smart.Server.Page              ( PageEither )
import qualified Smart.Server.Page             as SS.P
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Renderer.Utf8       ( renderMarkup )
import           Web.FormUrlEncoded             ( FromForm(..) )

type ServerSettings = '[SAuth.CookieSettings , SAuth.JWTSettings]

-- brittany-disable-next-binding 
type Exe = Auth.UserAuthentication :> Get '[B.HTML] (PageEither
               Pages.LandingPage
               (SS.P.Page 'SS.P.Authd User.UserProfile Pages.WelcomePage)
             )
             :<|> "forms" :> "login" :> Get '[B.HTML] Login.Page
             :<|> "forms" :> "signup" :> Get '[B.HTML] Signup.Page

             :<|> "echo" :> "login"
                  :> ReqBody '[FormUrlEncoded] Login.Input
                  :> Post '[B.HTML] Login.ResultPage
             :<|> "echo" :> "signup"
                  :> ReqBody '[FormUrlEncoded] User.Signup
                  :> Post '[B.HTML] Signup.ResultPage

             :<|> "login" :> Get '[B.HTML] Login.Page
             :<|> "signup" :> Get '[B.HTML] Signup.Page

             -- Temporarily let the old handlers in Public.hs use the "a" prefix.
             :<|> "b" :> "login"
                  :> ReqBody '[FormUrlEncoded] Login.Input
                  :> Post '[B.HTML] Login.ResultPage

             :<|> Public
             :<|> "private" :> Priv.Private
             :<|> Raw -- catchall for custom 404

exampleT :: forall m . Pub.PublicServerC m => ServerT Exe m
exampleT =
  showLandingPage
    :<|> documentLoginPage
    :<|> documentSignupPage
    :<|> echoLogin
    :<|> echoSignup
    :<|> showLoginPage
    :<|> showSignupPage
    :<|> handleLogin
    :<|> publicT
    :<|> Priv.privateT
    :<|> pure custom404

-- | Run as a Wai Application
exampleApplication
  :: forall m
   . Pub.PublicServerC m
  => (forall x . m x -> Handler x) -- ^ Natural transformation to transform an arbitrary @m@ to a Servant @Handler@
  -> Server.Context ServerSettings
  -> Wai.Application
exampleApplication handlerNatTrans ctx =
  Servant.serveWithContext exampleProxy ctx
    $ hoistServerWithContext exampleProxy settingsProxy handlerNatTrans exampleT
 where
  exampleProxy  = Proxy @Exe
  settingsProxy = Proxy @ServerSettings

runExeServer
  :: forall m
   . MonadIO m
  => Rt.Runtime -- ^ Runtime to use for running the server.
  -> m ()
runExeServer runtime@Rt.Runtime {..} = liftIO $ Warp.run port waiApp
 where
  Rt.ServerConf port = runtime ^. Rt.rConf . Rt.confServer
  waiApp =
    exampleApplication @Rt.ExeAppM (Rt.exampleAppMHandlerNatTrans runtime) ctx
  ctx =
    _rConf
      ^.        Rt.confCookie
      Server.:. _rJwtSettings
      Server.:. Server.EmptyContext

-- | Show the landing page when the user is not logged in, or the welcome page
-- when the user is logged in.
showLandingPage
  :: Pub.PublicServerC m
  => SAuth.AuthResult User.UserId
  -> m
       ( PageEither
           -- We don't use SS.P.Public Void to not have the automatic heading.
           Pages.LandingPage
           (SS.P.Page 'SS.P.Authd User.UserProfile Pages.WelcomePage)
       )
showLandingPage = \case
  SAuth.Authenticated userId ->
    S.dbSelect (User.SelectUserById userId) <&> headMay >>= \case
      Nothing -> do
        ML.warning "Cookie-based authentication succeeded, but the user ID is not found."
        authFailedErr $ "No user found with ID " <> show userId
      Just userProfile ->
        pure . SS.P.PageR $ SS.P.AuthdPage userProfile Pages.WelcomePage
  _ -> pure $ SS.P.PageL Pages.LandingPage
 where
  authFailedErr = Errs.throwError' . User.UserNotFound


--------------------------------------------------------------------------------
showSignupPage :: Pub.PublicServerC m => m Signup.Page
showSignupPage = pure $ Signup.Page "/a/signup"

documentSignupPage :: Pub.PublicServerC m => m Signup.Page
documentSignupPage = pure $ Signup.Page "/echo/signup"

echoSignup :: Pub.PublicServerC m => User.Signup -> m Signup.ResultPage
echoSignup input = pure $ Signup.Success $ show input


--------------------------------------------------------------------------------
showLoginPage :: Pub.PublicServerC m => m Login.Page
showLoginPage = pure $ Login.Page "/a/login"

documentLoginPage :: Pub.PublicServerC m => m Login.Page
documentLoginPage = pure $ Login.Page "/echo/login"

handleLogin :: Pub.PublicServerC m => Login.Input -> m Login.ResultPage
handleLogin _ = pure $ Login.Failure "TODO handleLogin"

echoLogin :: Pub.PublicServerC m => Login.Input -> m Login.ResultPage
echoLogin input = pure $ Login.Success $ show input


--------------------------------------------------------------------------------
custom404 :: Application
custom404 _request sendResponse = sendResponse $ Wai.responseLBS
  HTTP.status404
  [("Content-Type", "text/html; charset=UTF-8")]
  (renderMarkup $ H.toMarkup Pages.NotFoundPage)


--------------------------------------------------------------------------------
-- brittany-disable-next-binding
-- | A publicly available login page.
type Public = "a" :> "signup"
                 :> ReqBody '[FormUrlEncoded] User.Signup
                 :> Post '[B.HTML] (SS.P.Page 'SS.P.Public Void Pages.SignupResultPage)
            :<|>  "a" :> "login"
                  :> ReqBody '[FormUrlEncoded] User.UserCreds
                  :> Verb 'POST 303 '[JSON] ( Headers Auth.PostAuthHeaders
                                              NoContent
                                            )

publicT :: forall m . Pub.PublicServerC m => ServerT Public m
publicT = handleSignup :<|> authenticateUser

handleSignup (User.Signup username password email) = env $ do
  ML.info $ "Signing up new user: " <> show username <> "..."
  ids <- S.dbUpdate $ User.UserCreateGeneratingUserId username password email
  case headMay ids of
    Just uid -> do
      ML.info $ "User created: " <> show uid <> ". Sending success result."
      pure . SS.P.PublicPage $ Pages.SignupSuccess uid
    -- TODO Failure to create a user re-using an existing username doesn't
    -- trigger the Nothing case.
    Nothing  -> do
      -- TODO This should not be a 200 OK result.
      ML.info $ "Failed to create a user. Sending failure result."
      pure . SS.P.PublicPage $ Pages.SignupFailed "Failed to create users."
 where
  env = ML.localEnv (<> "HTTP" <> "Signup")

authenticateUser User.UserCreds {..} =
  env $ findMatchingUsers <&> headMay >>= \case
    Just u -> do
      ML.info "Found user, generating authentication cookies..."
      jwtSettings  <- asks Rt._rJwtSettings
      Rt.Conf {..} <- asks Rt._rConf
      mApplyCookies <- liftIO $ SAuth.acceptLogin
        _confCookie
        jwtSettings
        (u ^. User.userProfileId)
      ML.info "Applying cookies..."
      case mApplyCookies of
        Nothing -> do
          -- TODO What can cause a failure here ?
          ML.warning "Applying cookies failed. Sending failure result."
          unauthdErr _userCredsName
        Just applyCookies -> do
          ML.info "Cookies applied. Sending success result."
          pure . addHeader @"Location" "/" $ applyCookies
            NoContent
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
    S.dbSelect $ User.UserLoginWithUserName _userCredsName
                                            _userCredsPassword
  unauthdErr =
    Errs.throwError'
      . User.IncorrectPassword
      . mappend "User login failed: "
      . show
