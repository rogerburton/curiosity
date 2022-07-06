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
                  :> ReqBody '[FormUrlEncoded] Signup.Input
                  :> Post '[B.HTML] Signup.ResultPage

             :<|> "login" :> Get '[B.HTML] Login.Page
             :<|> "signup" :> Get '[B.HTML] Signup.Page

             -- Temporarily let the old handlers in Public.hs use the "a" prefix.
             :<|> "b" :> "login"
                  :> ReqBody '[FormUrlEncoded] Login.Input
                  :> Post '[B.HTML] Login.ResultPage
             :<|> "b" :> "signup"
                  :> ReqBody '[FormUrlEncoded] Signup.Input
                  :> Post '[B.HTML] Signup.ResultPage

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
    :<|> handleSignup
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
      -- TODO Log out the user.
      Nothing -> authFailedErr $ "No user found by ID = " <> show userId
      Just userProfile ->
        pure . SS.P.PageR $ SS.P.AuthdPage userProfile Pages.WelcomePage
  _ -> pure $ SS.P.PageL Pages.LandingPage
 where
  authFailedErr = Errs.throwError' . User.UserNotFound . mappend
    "Authentication failed, please login again. Error: "


--------------------------------------------------------------------------------
showSignupPage :: Pub.PublicServerC m => m Signup.Page
showSignupPage = pure $ Signup.Page "/a/signup"

documentSignupPage :: Pub.PublicServerC m => m Signup.Page
documentSignupPage = pure $ Signup.Page "/echo/signup"

handleSignup :: Pub.PublicServerC m => Signup.Input -> m Signup.ResultPage
handleSignup _ = pure $ Signup.Failure "TODO handleSignup"

echoSignup :: Pub.PublicServerC m => Signup.Input -> m Signup.ResultPage
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
type Public = "a" :> "login"
                  :> ReqBody '[FormUrlEncoded] User.UserCreds
                  :> Verb 'POST 303 '[JSON] ( Headers Auth.PostAuthHeaders
                                              NoContent
                                            )
            :<|> "a" :> "signup"
                 :> ReqBody '[FormUrlEncoded] User.CreateData
                 :> Post '[B.HTML] (SS.P.Page 'SS.P.Public Void Pages.SignupResultPage)

publicT :: forall m . Pub.PublicServerC m => ServerT Public m
publicT = authenticateUser :<|> processSignup

authenticateUser User.UserCreds {..} =
  env $ findMatchingUsers <&> headMay >>= \case
    Just u -> do
      -- get the config. to get the cookie and JWT settings.
      jwtSettings  <- asks Rt._rJwtSettings
      Rt.Conf {..} <- asks Rt._rConf
      ML.info "Found user, generating authentication cookies for the user."
      mApplyCookies <- liftIO $ SAuth.acceptLogin
        _confCookie
        jwtSettings
        (u ^. User.userProfileId)
      ML.info "Applying cookies."
      case mApplyCookies of
        Nothing -> do
          ML.warning "Auth failed."
          unauthdErr _userCredsName
        Just applyCookies -> do
          ML.info "User logged in"
          pure . addHeader @"Location" "/" $ applyCookies
            NoContent
    Nothing -> unauthdErr _userCredsName -- no users found
 where
  env               = ML.localEnv (<> "Login" <> show _userCredsName)
  findMatchingUsers = do
    ML.info "Login with UserId failed, falling back to UserName based auth."
    S.dbSelect $ User.UserLoginWithUserName _userCredsName
                                            _userCredsPassword
  unauthdErr =
    Errs.throwError'
      . User.IncorrectPassword
      . mappend "User login failed: "
      . show

processSignup (User.CreateData userName password) = env $ do
  ML.info $ "Signing up new user: " <> show userName <> "..."
  ids <- S.dbUpdate $ User.UserCreateGeneratingUserId userName password
  ML.info $ "Users created: " <> show ids
  pure . SS.P.PublicPage $ case headMay ids of
    Just uid -> Pages.SignupSuccess uid
    Nothing  -> Pages.SignupFailed "Failed to create users."
  where env = ML.localEnv (<> "Signup")
