{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds
           , TypeOperators
#-} -- Language extensions needed for servant.
{- |
Module: Prototype.Example.Server
Description: Server root module, split up into public and private sub-modules.

-}
module Prototype.Example.Server
  (
    -- * Top level server types.
    Example
  , exampleT
  , exampleApplication
  , runExampleServer
  -- * Type-aliases for convenience
  , ServerSettings
  ) where

import           Control.Lens
import qualified Network.HTTP.Types            as HTTP
import qualified Network.Wai                   as Wai
import qualified Network.Wai.Handler.Warp      as Warp
import qualified Prototype.Example.Runtime     as Rt
import qualified Prototype.Example.Server.Public
                                               as Pub
import qualified Prototype.Example.Server.Public.Pages
                                               as Pages
import           Servant
import qualified Servant.Auth.Server           as Srv
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Renderer.Utf8       ( renderMarkup )

type ServerSettings = '[Srv.CookieSettings , Srv.JWTSettings]

type Example = "public" :> Pub.Public
             :<|> Raw -- catchall for custom 404

exampleT :: forall m . Pub.PublicServerC m => ServerT Example m
exampleT = Pub.publicT :<|> pure custom404

-- | Run as a Wai Application
exampleApplication
  :: forall m
   . Pub.PublicServerC m
  => (forall x . m x -> Handler x) -- ^ Natural transformation to transform an arbitrary @m@ to a Servant @Handler@
  -> Wai.Application
exampleApplication handlerNatTrans =
  Servant.serve exampleProxy $ hoistServerWithContext exampleProxy
                                                      settingsProxy
                                                      handlerNatTrans
                                                      exampleT
 where
  exampleProxy  = Proxy @Example
  settingsProxy = Proxy @ServerSettings

runExampleServer
  :: forall m
   . MonadIO m
  => Rt.Runtime -- ^ Runtime to use for running the server.
  -> m ()
runExampleServer runtime = liftIO $ Warp.run port waiApp
 where
  Rt.ServerConf port = runtime ^. Rt.rConf . Rt.confServer
  waiApp =
    exampleApplication @Rt.ExampleAppM $ Rt.exampleAppMHandlerNatTrans runtime

custom404 :: Application
custom404 _request sendResponse = sendResponse $ Wai.responseLBS
  HTTP.status404
  [("Content-Type", "text/html; charset=UTF-8")]
  (renderMarkup $ H.toMarkup Pages.NotFoundPage)
