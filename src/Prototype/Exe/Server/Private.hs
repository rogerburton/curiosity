{-# LANGUAGE ConstraintKinds #-}
{- |
Module: Prototype.Exe.Server.Private
Description: Private endpoints

Contains the public endpoints for the example server.
We're using PackageImports here on purpose: this includes imports from @start-servant@ and those imports are tagged for readability
and predictability on where these modules come from.

-}
module Prototype.Exe.Server.Private
  ( PrivateServerC
  ) where

import qualified Commence.Multilogging         as ML
import qualified Commence.Runtime.Storage      as S
import "exceptions" Control.Monad.Catch         ( MonadMask )
import qualified Prototype.Exe.Data.User       as User
import qualified Prototype.Exe.Runtime         as Rt


--------------------------------------------------------------------------------
type PrivateServerC m
  = ( MonadMask m
    , ML.MonadAppNameLogMulti m
    , S.DBStorage m User.UserProfile
    , MonadReader Rt.Runtime m
    , MonadIO m
    )
