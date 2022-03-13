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

import qualified "start-servant" MultiLogging  as L
import qualified Network.Wai                   as Wai
import qualified Prototype.Example.Server.Public.Pages
                                               as Pages
import qualified "start-servant" Prototype.Server.New.Page
                                               as SS.P
import           Servant
import qualified Servant.HTML.Blaze            as B

-- | Minimal set of constraints needed on some monad @m@ to be satisfied to be able to run a public server. 
type PublicServerC m = (Applicative m, L.MonadAppNameLogMulti m)

-- | A publicly available login page. 
type Public = "login" :> Get '[B.HTML] (SS.P.Page 'SS.P.Public Pages.LoginPage) -- fixme: use the login page from start-servant for now. 

publicT :: forall m . PublicServerC m => ServerT Public m
publicT = showLoginPage
 where
  showLoginPage = pure . SS.P.PublicPage $ Pages.LoginPage "./authenticate"

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
