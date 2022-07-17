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

import qualified Prototype.Exe.Data.User       as User
import           Smart.Html.Avatar
import qualified Smart.Html.Dsl                as Dsl
import           Smart.Html.Navbar
import qualified Smart.Html.Render             as Render
import           Smart.Html.Shared.Html.Icons   ( svgIconAdd
                                                , svgIconArrowRight
                                                )
import           Text.Blaze                     ( customAttribute )
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A
import           Web.FormUrlEncoded             ( FromForm(..)
                                                , parseMaybe
                                                )


--------------------------------------------------------------------------------
-- | A simple welcome page.
data WelcomePage = WelcomePage

instance H.ToMarkup WelcomePage where
  toMarkup _ =
    Render.renderCanvas
      . Dsl.SingletonCanvas
      $ H.div
      ! A.class_ "c-app-layout"
      $ H.header
      $ H.toMarkup exampleNavbarAlt


--------------------------------------------------------------------------------
data ProfilePage = ProfilePage
  { _profilePageUserProfile :: User.UserProfile
  , _profilePageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup ProfilePage where
  toMarkup (ProfilePage profile submitUrl) =
    Render.renderCanvas
      . Dsl.SingletonCanvas
      $ H.div
      ! A.class_ "c-app-layout"
      $ do
          H.header $ H.toMarkup exampleNavbarAlt
          H.main ! A.class_ "u-maximize-width u-scroll-wrapper" $ profileForm
            profile
            submitUrl

profileForm profile submitUrl = do
  H.div
    ! A.class_ "c-navbar c-navbar--bordered-bottom"
    $ H.div
    ! A.class_ "c-toolbar"
    $ H.div
    ! A.class_ "c-toolbar__left"
    $ H.div
    ! A.class_ "c-toolbar__item"
    $ H.h2
    ! A.class_ "c-toolbar__title"
    $ "User profile"
  H.div
    ! A.class_ "u-scroll-wrapper-body"
    $ H.div
    ! A.class_ "o-container o-container--large"
    $ H.div
    ! A.class_ "o-container-vertical"
    $ H.div
    ! A.class_ "o-container o-container--medium"
    $ H.div
    ! A.class_ "o-form-group-layout o-form-group-layout--horizontal"
    $ H.form
    $ do
        H.div ! A.class_ "o-form-group" $ do
          H.label
            ! A.class_ "o-form-group__label"
            ! A.for "username"
            $ "Username"
          H.div
            ! A.class_
                "o-form-group__controls o-form-group__controls--full-width"
            $ do
                H.input
                  ! A.class_ "c-input"
                  ! A.id "username"
                  ! A.name "username"
                  ! A.value
                      ( H.toValue @Text
                      . show
                      . User._userCredsName
                      . User._userProfileCreds
                      $ profile
                      )
                H.p
                  ! A.class_ "c-form-help-text"
                  $ "This is your public username"
        H.div ! A.class_ "o-form-group" $ do
          H.label
            ! A.class_ "o-form-group__label"
            ! A.for "password"
            $ "Password"
          H.div
            ! A.class_
                "o-form-group__controls o-form-group__controls--full-width"
            $ do
                H.input
                  ! A.class_ "c-input"
                  ! A.type_ "password"
                  ! A.id "password"
                  ! A.name "password"
        H.div ! A.class_ "o-form-group" $ do
          H.label
            ! A.class_ "o-form-group__label"
            ! A.for "display-name"
            $ "Display name"
          H.div
            ! A.class_
                "o-form-group__controls o-form-group__controls--full-width"
            $ do
                H.input
                  ! A.class_ "c-input"
                  ! A.id "display-name"
                  ! A.name "display-name"
                  ! A.value
                      ( H.toValue @Text
                      . show
                      . User._userProfileDisplayName
                      $ profile
                      )
                H.p
                  ! A.class_ "c-form-help-text"
                  $ "This is the name that appears in e.g. your public profile"
        H.div ! A.class_ "o-form-group" $ do
          H.label
            ! A.class_ "o-form-group__label"
            ! A.for "email-addr"
            $ "Email address"
          H.div
            ! A.class_
                "o-form-group__controls o-form-group__controls--full-width"
            $ do
                H.input
                  ! A.class_ "c-input"
                  ! A.id "email-addr"
                  ! A.name "email-addr"
                  ! A.value
                      ( H.toValue @Text
                      . show
                      . User._userProfileEmailAddr
                      $ profile
                      )
                H.p
                  ! A.class_ "c-form-help-text"
                  $ "Your email address is private"
        H.div
          ! A.class_ "o-form-group"
          $ H.div
          ! A.class_ "u-spacer-left-auto"
          $ H.button
          ! A.class_ "c-button c-button--primary"
          ! A.formaction (H.toValue submitUrl)
          ! A.formmethod "POST"
          $ H.span
          ! A.class_ "c-button__content"
          $ do
              H.span ! A.class_ "c-button__label" $ "Update profile"

-- Partial re-creation of
-- https://design.smart.coop/prototypes/old-desk/contract-create-1.html
-- TODO Move to smart-design-hs and refactor.
contractCreate1 =
  H.div
    ! A.class_ "u-scroll-wrapper-body"
    $ H.div
    ! A.class_ "o-container o-container--large"
    $ H.div
    ! A.class_ "o-container-vertical"
    $ H.div
    ! A.class_ "o-container o-container--medium"
    $ H.div
    ! A.class_ "o-form-group-layout o-form-group-layout--horizontal"
    $ do
        H.div ! A.class_ "o-form-group" $ do
          H.label ! A.class_ "o-form-group__label" ! A.for "project" $ "Project"
          H.div
            ! A.class_
                "o-form-group__controls o-form-group__controls--full-width"
            $ do
                H.button
                  ! A.class_ "c-select-custom"
                  ! customAttribute "aria-haspopup"       "listbox"
                  ! customAttribute "data-menu"           "projects"
                  ! customAttribute "data-menu-samewidth" "true"
                  ! customAttribute "aria-expanded"       "false"
                  $ H.div
                  ! A.class_ "c-select-custom__value"
                  $ "Select project"
                H.ul
                  ! A.class_ "c-menu c-menu--select-custom"
                  ! A.role "listbox"
                  ! A.id "projects"
                  $ do
                      H.li
                        ! A.class_ "c-menu__item"
                        ! A.role "option"
                        $ H.div
                        ! A.class_ "c-menu__label"
                        $ "Project 1"
                      H.li
                        ! A.class_ "c-menu__item"
                        ! A.role "option"
                        $ H.div
                        ! A.class_ "c-menu__label"
                        $ "Project 2"
                      H.li
                        ! A.class_ "c-menu__item"
                        ! A.role "option"
                        $ H.div
                        ! A.class_ "c-menu__label"
                        $ "Project 3"
                      H.li ! A.class_ "c-menu__divider" $ ""
                      H.li
                        ! A.class_ "c-menu__item"
                        $ H.div
                        ! A.class_ "c-menu__label"
                        $ do
                            H.div
                              ! A.class_ "o-svg-icon o-svg-icon-add  "
                              $ H.toMarkup svgIconAdd
                            H.span "Add new project"
                H.p
                  ! A.class_ "c-form-help-text"
                  $ "Enter an existing project or create new one"
        H.div ! A.class_ "o-form-group" $ do
          H.label
            ! A.class_ "o-form-group__label"
            ! A.for "description"
            $ "Description"
          H.div
            ! A.class_
                "o-form-group__controls o-form-group__controls--full-width"
            $ do
                H.textarea
                  ! A.class_ "c-textarea"
                  ! A.rows "5"
                  ! A.id "description"
                  $ ""
                H.p
                  ! A.class_ "c-form-help-text"
                  $ "Describe your work (minimum 10 characters)"
        H.div
          ! A.class_ "o-form-group"
          $ H.div
          ! A.class_ "u-spacer-left-auto"
          $ H.button
          ! A.class_ "c-button c-button--primary"
          ! A.type_ "button"
          $ H.span
          ! A.class_ "c-button__content"
          $ do
              H.span ! A.class_ "c-button__label" $ "Next"
              H.div
                ! A.class_ "o-svg-icon o-svg-icon-arrow-right  "
                $ H.toMarkup svgIconArrowRight


newtype EditProfileForm = EditProfileForm
  { _editPassword :: Maybe User.Password
  }
  deriving (Eq, Show, Generic)

instance FromForm EditProfileForm where
  fromForm f = EditProfileForm <$> parseMaybe "password" f

data ProfileSaveConfirmPage = ProfileSaveSuccess
                            | ProfileSaveFailure (Maybe Text)
                            deriving Show

instance H.ToMarkup ProfileSaveConfirmPage where
  toMarkup = \case
    ProfileSaveSuccess      -> "All done, you can now go back."
    ProfileSaveFailure mmsg -> do
      H.text $ "We had a problem saving your data."
      maybe mempty (H.text . mappend "Reason: ") mmsg


--------------------------------------------------------------------------------
exampleNavbarAlt :: Navbar
exampleNavbarAlt = Navbar [] [userEntry]

userEntry = UserEntry userEntries NoAvatarImage

userEntries = [SubEntry "Settings" "/settings/profile" False]
