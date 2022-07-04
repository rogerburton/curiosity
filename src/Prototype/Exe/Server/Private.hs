{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{- |
Module: Prototype.Exe.Server.Private
Description: Private endpoints

Contains the public endpoints for the example server.
We're using PackageImports here on purpose: this includes imports from @start-servant@ and those imports are tagged for readability
and predictability on where these modules come from.

-}
module Prototype.Exe.Server.Private
  ( Private
  , privateT
  , PrivateServerC
  ) where

import qualified Commence.Multilogging         as ML
import qualified Commence.Runtime.Errors       as Errs
import qualified Commence.Runtime.Storage      as S
import           Control.Lens
import "exceptions" Control.Monad.Catch         ( MonadMask )
import qualified Prototype.Exe.Data.User       as User
import qualified Prototype.Exe.Runtime         as Rt
import qualified Prototype.Exe.Server.Private.Auth
                                               as Auth
import qualified Prototype.Exe.Server.Private.Helpers
                                               as H
import qualified Prototype.Exe.Server.Private.Pages
                                               as Pages
import           Servant
import qualified Servant.Auth.Server           as SAuth
import qualified Smart.Server.Page             as SS.P

type PrivateServerC m
  = ( MonadMask m
    , ML.MonadAppNameLogMulti m
    , S.DBStorage m User.UserProfile
    , MonadReader Rt.Runtime m
    , MonadIO m
    )

-- brittany-disable-next-binding
-- | The private API with authentication.
type Private = Auth.UserAuthentication :> UserPages
-- brittany-disable-next-binding
type UserPages
  = "welcome" :> H.GetUserPage Pages.WelcomePage
    :<|> "user" :> "profile" :> H.GetUserPage Pages.ProfilePage
    :<|> EditUser

privateT :: forall m . PrivateServerC m => ServerT Private m
privateT authResult = showWelcomePage :<|> showProfilePage :<|> editUser
 where
  showWelcomePage =
    withUser $ \profile -> pure $ SS.P.AuthdPage profile Pages.WelcomePage
  showProfilePage = withUser $ \profile ->
    pure . SS.P.AuthdPage profile . Pages.ProfilePage $ "./profile"
  editUser Pages.EditProfileForm {..} = withUser $ \profile ->
    case _editPassword of
      Just newPass ->
        let updatedProfile =
              profile
                &  User.userProfileCreds
                .  User.userCredsPassword
                %~ (`fromMaybe` _editPassword)
        in  S.dbUpdate (User.UserPasswordUpdate (S.dbId profile) newPass)
              <&> headMay
              <&> SS.P.AuthdPage updatedProfile
              .   \case
                    Nothing -> Pages.ProfileSaveFailure
                      $ Just "Empty list of affected User IDs on update."
                    Just{} -> Pages.ProfileSaveSuccess
      Nothing ->
        pure . SS.P.AuthdPage profile . Pages.ProfileSaveFailure $ Just
          "Nothing to update."

  -- extract the user from the authentication result or throw an error.
  withUser :: forall a . (User.UserProfile -> m a) -> m a
  withUser f = case authResult of
    SAuth.Authenticated userId ->
      S.dbSelect (User.SelectUserById userId) <&> headMay >>= \case
        Nothing -> authFailedErr $ "No user found by ID = " <> show userId
        Just userProfile -> f userProfile
    authFailed -> authFailedErr $ show authFailed
   where
    authFailedErr = Errs.throwError' . User.UserNotFound . mappend
      "Authentication failed, please login again. Error: "

-- brittany-disable-next-binding
type EditUser = "user"
                :> "profile"
                :> ReqBody '[FormUrlEncoded] Pages.EditProfileForm
                :> H.PostUserPage Pages.ProfileSaveConfirmPage
