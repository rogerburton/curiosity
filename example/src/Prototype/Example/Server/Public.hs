{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{- |
Module: Prototype.Example.Server.Public
Description: Public endpoints

Contains the public endpoints for the example server.
We're using PackageImports here on purpose: this includes imports from @start-servant@ and those imports are tagged for readability
and predictability on where these modules come from. 

-}
module Prototype.Example.Server.Public
  ( Public
  , publicT
  , publicApplication
  , PublicServerC
  ) where

import           Control.Lens
import "exceptions" Control.Monad.Catch         ( MonadMask )
import qualified "start-servant" MultiLogging  as ML
import qualified Network.Wai                   as Wai
import qualified Prototype.Example.Data.User   as User
import qualified Prototype.Example.Runtime     as Rt
import qualified Prototype.Example.Server.Public.Pages
                                               as Pages
import qualified "start-servant" Prototype.Lib.Wrapped
                                               as W
import qualified Prototype.Runtime.Errors      as Errs
import qualified Prototype.Runtime.Storage     as S
import qualified Prototype.Server.New.Auth     as Auth
import qualified "start-servant" Prototype.Server.New.Page
                                               as SS.P
import           Servant
import qualified Servant.Auth.Server           as SAuth
import qualified Servant.HTML.Blaze            as B
import qualified Web.FormUrlEncoded            as Form

-- | Minimal set of constraints needed on some monad @m@ to be satisfied to be able to run a public server. 
type PublicServerC m
  = ( MonadMask m
    , ML.MonadAppNameLogMulti m
    , S.DBStorage m User.UserProfile
    , MonadReader Rt.Runtime m
    , MonadIO m
    )

-- brittany-disable-next-binding 
-- | A publicly available login page. 
type Public = "login" :> (  Get '[B.HTML] (SS.P.Page 'SS.P.Public Pages.LoginPage)
                       :<|> "authenticate" :> ReqBody '[FormUrlEncoded] User.UserCreds
                           :> Verb 'POST 303 '[JSON] ( Headers Auth.PostAuthHeaders
                                                       NoContent
                                                     )
                         )
            :<|> "signup" :> ( Get '[B.HTML] (SS.P.Page 'SS.P.Public Pages.SignupPage) -- display the signup page. 
                           :<|> "create" :> ReqBody '[FormUrlEncoded] User.UserPassword
                                         :> ReqBody '[FormUrlEncoded] (W.Wrapped "password_confirmation" User.UserPassword)
                                         :> Post '[B.HTML] (SS.P.Page 'SS.P.Public Pages.SignupResultPage) -- create the user. 
                             )

publicT :: forall m . PublicServerC m => ServerT Public m
publicT =
  (showLoginPage :<|> authenticateUser) :<|> (showSignupPage :<|> processSignup)
 where
  showLoginPage = pure . SS.P.PublicPage $ Pages.LoginPage "./authenticate"
  authenticateUser creds =
    S.dbSelect (User.UserLogin creds) <&> headMay >>= \case
      Just u -> do
        -- get the config. to get the cookie and JWT settings. 
        Rt.Conf {..}  <- asks Rt._rConf
        jwtSettings   <- asks Rt._rJwtSettings
        mApplyCookies <- liftIO $ SAuth.acceptLogin
          _confCookie
          jwtSettings
          (u ^. User.userCreds . User.userCredsId)
        case mApplyCookies of
          Nothing -> unauthdErr $ u ^. User.userCreds . User.userCredsId
          Just applyCookies -> do
            ML.info "User logged in"
            pure . addHeader @"Location" "/private/welcome" $ applyCookies
              NoContent

      Nothing -> unauthdErr $ creds ^. User.userCredsId -- no users found 
  showSignupPage = pure . SS.P.PublicPage $ Pages.SignupPage "./create"
  processSignup  = undefined
  unauthdErr     = Errs.throwError' . User.IncorrectPassword . show

-- | Run as a Wai Application 
publicApplication
  :: forall m
   . PublicServerC m
  => (forall x . m x -> Handler x) -- ^ Natural transformation to transform an arbitrary @m@ to a Servant @Handler@
  -> Wai.Application
publicApplication handlerNatTrans = Servant.serve
  pubPxy
  (hoistServer pubPxy handlerNatTrans publicT {- ServerT -> Server -}
                                             )
  where pubPxy = Proxy @Public
