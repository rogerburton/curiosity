{-# LANGUAGE DeriveAnyClass #-}
module Curiosity.Form.Signup
  ( Page(..)
  , SignupResultPage(..)
  ) where

import           Smart.Html.Dsl                 ( HtmlCanvas )
import qualified Smart.Html.Dsl                as Dsl
import qualified Smart.Html.Render             as Render
import           Smart.Html.Shared.Html.Icons
import qualified Smart.Html.Shared.Types       as HTypes
import           Text.Blaze                     ( customAttribute )
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!), Html )
import qualified Text.Blaze.Html5.Attributes   as A


--------------------------------------------------------------------------------
data Page = Page
  { signupSubmitURL :: H.AttributeValue
  }

instance H.ToMarkup Page where
  toMarkup = Render.renderCanvas . signupPage

signupPage :: Page -> HtmlCanvas
signupPage Page {..} = Dsl.SingletonCanvas $ do
  H.header
    ! A.class_ "o-container-vertical"
    $ H.div
    ! A.class_ "o-container"
    $ H.div
    ! A.class_ "c-brand c-brand--xsmall"
    $ H.a
    ! A.href "/"
    $ H.img
    ! A.src "/static/images/logo.svg"
    ! A.alt "Smart"
  H.main
    ! A.class_ "o-container-vertical"
    $ H.div
    ! A.class_ "o-container o-container--small"
    $ do
        H.div
          ! A.class_ "c-panel"
          $ H.div
          ! A.class_ "c-panel__body"
          $ H.form
          $ do
              H.h3 ! A.class_ "c-h2" $ "Create your account"
              H.div
                ! A.class_ "o-form-group-layout o-form-group-layout--standard"
                $ do
                    H.div ! A.class_ "o-form-group" $ do
                      H.label
                        ! A.class_ "o-form-group__label"
                        ! A.for "username"
                        $ "Username"
                      H.div
                        ! A.class_ "o-form-group__controls"
                        $ H.input
                        ! A.class_ "c-input"
                        ! A.id "username"
                        ! A.name "username"
                        ! A.type_ "text"
                    H.div ! A.class_ "o-form-group" $ do
                      H.label
                        ! A.class_ "o-form-group__label"
                        ! A.for "email-addr"
                        $ "Email address"
                      H.div
                        ! A.class_ "o-form-group__controls"
                        $ H.input
                        ! A.class_ "c-input"
                        ! A.id "email-addr"
                        ! A.name "email-addr"
                        ! A.type_ "email"
                    H.div ! A.class_ "o-form-group" $ do
                      H.label
                        ! A.class_ "o-form-group__label"
                        ! A.for "password"
                        $ "Password"
                      H.div
                        ! A.class_ "o-form-group__controls"
                        $ H.div
                        ! A.class_ "c-input-with-icon"
                        $ do
                            H.input
                              ! A.class_ "c-input"
                              ! A.id "password"
                              ! A.name "password"
                              ! A.type_ "password"
                            H.button
                              ! A.class_ "c-input-with-icon__toggle"
                              ! customAttribute "data-password-toggle" "1"
                              $ do
                                  H.div
                                    ! A.class_ "o-svg-icon o-svg-icon-eye"
                                    $ H.toMarkup svgIconEye
                                  H.div
                                    ! A.class_ "o-svg-icon o-svg-icon-eye-off"
                                    $ H.toMarkup svgIconEyeOff
                    H.div ! A.class_ "o-form-group" $ do
                      H.div ! A.class_ "c-checkbox" $ do
                        H.label $ do
                          H.input
                            ! A.id "tos-consent"
                            ! A.name "tos-consent"
                            ! A.type_ "checkbox"
                            ! A.value "tos-consent-granted"
                          H.div $ do
                            "I understand that this site is up for "
                            "demonstration purpose only, and that data is "
                            "regularly erased."
                            H.a ! A.href "#" $ "Read more."
                    H.div
                      ! A.class_ "o-form-group"
                      $ H.button
                      ! A.class_ "c-button c-button--primary c-button--block"
                      ! A.formaction signupSubmitURL
                      ! A.formmethod "POST"
                      $ H.span
                      ! A.class_ "c-button__content"
                      $ "Sign up"
                    H.div ! A.class_ "o-form-group u-ta-center" $ ""
        H.div ! A.class_ "c-content u-text-center u-spacer-top-l" $ do
          H.a ! A.class_ "u-text-muted" ! A.href "/login" $ "Log in"


--------------------------------------------------------------------------------
data SignupResultPage = SignupSuccess
                      | SignupFailed Text

instance H.ToMarkup SignupResultPage where
  toMarkup = \case
    SignupSuccess ->
      Render.renderCanvas
        $ Dsl.SingletonCanvas
        $ withMessage "Sign up successful"
        $ H.p
        $ do
            "You can now proceed to "
            H.a ! A.href "/login" $ "log in"
            "."
    SignupFailed msg -> withText $ "Failed sign up: " <> msg
   where
    withText msg =
      H.toMarkup @Dsl.HtmlCanvas $ Dsl.SingletonCanvas (HTypes.Title $ msg)

withMessage :: Text -> Html -> Html
withMessage title msg = do
  H.header
    $
    -- TODO The main header bottom border is not aligned with a regular
    -- navigation bar.  (But here we don't display the border, so it's not really
    -- visible. This is based on a Dialog component. Maybe this should be based
    -- on a normal page instead. There is also an auto-focus thing going on.
      H.div
    ! A.class_ "c-dialog"
    ! A.role "dialog"
    ! customAttribute "aria-labelledby" "dialogTitle-04"
    $ do
        H.div
          ! A.class_ "c-dialog__header c-dialog__header"
          $ H.div
          ! A.class_ "c-toolbar c-toolbar--spaced"
          $ do
              H.div
                ! A.class_ "c-toolbar__left"
                $ H.div
                ! A.class_ "c-toolbar__item"
                $ H.h2
                ! A.class_ "c-dialog__title"
                ! A.id "dialogTitle-04"
                $ H.div
                ! A.class_ "c-brand c-brand--xsmall"
                $ H.a
                ! A.href "/"
                $ H.img
                ! A.src "/static/images/logo.svg"
                ! A.alt "Smart"
              H.div
                ! A.class_ "c-toolbar__right"
                $ H.div
                ! A.class_ "c-toolbar__item"
                $ H.a
                ! A.href "/login"
                ! A.class_ "c-button c-button--borderless c-button--icon"
                ! customAttribute "data-dialog-close" "data-dialog-close"
                ! customAttribute "aria-label"        "Close dialog"
                $ H.div
                ! A.class_ "c-button__content"
                $ H.div
                ! A.class_ "o-svg-icon o-svg-icon-close"
                $ H.toMarkup svgIconClose
  H.main
    $ H.div
    ! A.class_ "u-scroll-wrapper-body"
    $ H.div
    ! A.class_ "o-container o-container--large"
    $ H.div
    ! A.class_ "o-container-vertical"
    $ do
        H.div ! A.class_ "c-content" $ H.h1 $ H.toHtml title

        H.div ! A.class_ "c-panel" $ H.div ! A.class_ "c-panel__body" $ msg
