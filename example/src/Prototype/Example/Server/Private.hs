{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{- |
Module: Prototype.Example.Server.Private
Description: Private endpoints

Contains the public endpoints for the example server.
We're using PackageImports here on purpose: this includes imports from @start-servant@ and those imports are tagged for readability
and predictability on where these modules come from.

-}
module Prototype.Example.Server.Private
  ( Private
  , privateT
  , privateApplication
  , PrivateServerC
  ) where

import           Control.Lens
import "exceptions" Control.Monad.Catch         ( MonadMask )
import qualified "start-servant" MultiLogging  as ML
import qualified Network.Wai                   as Wai
import qualified Prototype.Example.Data.User   as User
import qualified Prototype.Example.Runtime     as Rt
import qualified Prototype.Example.Server.Private.Pages
                                               as Pages
import qualified Prototype.Runtime.Errors      as Errs
import qualified Prototype.Runtime.Storage     as S
import qualified Prototype.Server.New.Auth     as Auth
import qualified "start-servant" Prototype.Server.New.Page
                                               as SS.P
import           Servant
import qualified Servant.Auth.Server           as SAuth
import qualified Servant.HTML.Blaze            as B
import           Web.FormUrlEncoded             ( FromForm(..) )

type PrivateServerC m
  = ( MonadMask m
    , ML.MonadAppNameLogMulti m
    , S.DBStorage m User.UserProfile
    , MonadReader Rt.Runtime m
    , MonadIO m
    )

-- | The private API with authentication.
type Private
  = "welcome" :> Get '[B.HTML] (SS.P.Page 'SS.P.Authd Pages.WelcomePage)

privateT = undefined
privateApplication = undefined
