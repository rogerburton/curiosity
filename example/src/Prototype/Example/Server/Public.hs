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

import qualified Network.Wai                   as Wai
import qualified "start-servant" Prototype.Server.New.Page
                                               as P
import           Servant
import qualified Servant.HTML.Blaze            as B

-- | Minimal set of constraints needed on some monad @m@ to be satisfied to be able to run a public server. 
type PublicServerC m = (Applicative m)

-- | A publicly available login page. 
type Public = "login" :> Get '[B.HTML] (P.Page 'P.Public P.LoginPage) -- fixme: use the login page from start-servant for now. 

publicT :: forall m . Applicative m => ServerT Public m
publicT = showLoginPage
 where
  showLoginPage = pure . P.PublicPage $ P.LoginPage "public/login/authenticate"

-- | Run as a Wai Application 
publicApplication
  :: forall m
   . Applicative m
  => (forall x . m x -> Handler x) -- ^ Natural transformation to transform an arbitrary @m@ to a Servant @Handler@
  -> Wai.Application
publicApplication handlerNatTrans = Servant.serve
  pubPxy
  (hoistServer pubPxy handlerNatTrans publicT {- ServerT -> Server -}
                                             )
  where pubPxy = Proxy @Public
