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

import Control.Lens 
import qualified Prototype.Server.New.Auth     as Auth
import qualified Prototype.Example.Data.User as User  
import qualified "start-servant" MultiLogging  as L
import qualified Network.Wai                   as Wai
import qualified Prototype.Example.Server.Public.Pages
                                               as Pages
import qualified Servant.Auth.Server           as SAuth
import qualified Prototype.Runtime.Storage     as S
import qualified "start-servant" Prototype.Server.New.Page
                                               as SS.P
import           Servant
import qualified Servant.HTML.Blaze            as B
import qualified Prototype.Example.Runtime as Rt

-- | Minimal set of constraints needed on some monad @m@ to be satisfied to be able to run a public server. 
type PublicServerC m = (Applicative m, L.MonadAppNameLogMulti m, S.DBStorage m User.UserProfile, MonadReader Rt.Runtime m, MonadIO m)

-- | A publicly available login page. 
type Public = "login" :> (  Get '[B.HTML] (SS.P.Page 'SS.P.Public Pages.LoginPage)
                       :<|> "authenticate" :> ReqBody '[FormUrlEncoded] User.UserCreds 
                           :> Verb 'POST 303 '[JSON] ( Headers Auth.PostAuthHeaders
                                                       NoContent
                                                     )
                         )

publicT :: forall m . PublicServerC m => ServerT Public m
publicT = showLoginPage :<|> authenticateUser
 where
  showLoginPage = pure . SS.P.PublicPage $ Pages.LoginPage "./authenticate"
  unauthdErr = undefined 
  authenticateUser creds =
    S.dbSelect (User.UserLogin creds) <&> headMay >>= \case
    Just u ->    do
      -- get the config. to get the cookie and JWT settings. 
      Rt.Conf {..}  <- asks Rt._rConf
      jwtSettings   <- asks Rt._rJwtSettings
      mApplyCookies <- liftIO
        $ SAuth.acceptLogin _confCookie jwtSettings (u ^. User.userCreds . User.userCredsId)
      case mApplyCookies of
        Nothing           -> unauthdErr
        Just applyCookies -> do
          -- Logging.info "User logged in"
          pure . addHeader @"Location" "/private/welcome" $ applyCookies
            NoContent

    Nothing  -> undefined -- no users found 
 

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
