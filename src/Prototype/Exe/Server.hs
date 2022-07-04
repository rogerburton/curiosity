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

             :<|> "a" :> "login"
                  :> ReqBody '[FormUrlEncoded] Login.Input
                  :> Post '[B.HTML] Login.ResultPage
             :<|> "a" :> "signup"
                  :> ReqBody '[FormUrlEncoded] Signup.Input
                  :> Post '[B.HTML] Signup.ResultPage

             :<|> "public" :> Pub.Public
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
    :<|> Pub.publicT
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
