{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{- |
Module: Prototype.Exe.Server.Private.Pages
Description: Private pages for the application

The goal is to exemplify the use of our DSL for some simple pages and to have something more tangible to show.

-}
module Prototype.Exe.Server.Private.Pages
  ( WelcomePage(..)
  , ProfilePage(..)
  , EditProfileForm(..)
  , ProfileSaveConfirmPage(..)
  ) where

import           Network.HTTP.Types.Method
import qualified Prototype.Exe.Data.User       as User
import           Prototype.Exe.Server.Shared.Html.Helpers.Form
                                                ( mkButton )
import qualified "design-hs-lib" Smart.Html.Dsl
                                               as Dsl
import qualified "design-hs-lib" Smart.Html.Form
                                               as Form
import qualified "design-hs-lib" Smart.Html.Input
                                               as Inp
import qualified "design-hs-lib" Smart.Html.Shared.Types
                                               as HTypes
import qualified Text.Blaze.Html5              as H
import           Web.FormUrlEncoded             ( FromForm(..) )

-- | A simple welcome page. 
data WelcomePage = WelcomePage

instance H.ToMarkup WelcomePage where
  toMarkup _ = "Welcome to the welcome page!"

newtype ProfilePage = ProfilePage { _profilePageSubmitURL :: H.AttributeValue }

instance H.ToMarkup ProfilePage where
  toMarkup (ProfilePage submitUrl) = do
    H.title "Edit user profile."
    H.form
      . H.toMarkup @Dsl.HtmlCanvas
      $ (       Form.InputGroup [username, password]
        Dsl.::~ mkButton "Update" submitUrl POST
        Dsl.::~ Dsl.EmptyCanvas
        )

   where
    username =
      ( "Username"
      , Inp.PlainTextInput HTypes.Enabled
                           "_editUserName"
                           "_editUserName"
                           Nothing
      )
    password =
      ( "Password"
      , Inp.PasswordInput HTypes.Enabled "_editPassword" "_editPassword" Nothing
      )

newtype EditProfileForm = EditProfileForm
  { _editPassword :: Maybe User.Password
  }
  deriving (Eq, Show, Generic)
  deriving anyclass FromForm

data ProfileSaveConfirmPage = ProfileSaveSuccess
                            | ProfileSaveFailure (Maybe Text)
                            deriving Show

instance H.ToMarkup ProfileSaveConfirmPage where
  toMarkup = \case
    ProfileSaveSuccess      -> "All done, you can now go back."
    ProfileSaveFailure mmsg -> do
      H.text $ "We had a problem saving your data."
      maybe mempty (H.text . mappend "Reason: ") mmsg
