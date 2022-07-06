{-# LANGUAGE DeriveAnyClass #-}
module Prototype.Exe.Form.Signup
  ( Page(..)
  , ResultPage(..)
  ) where

import           Smart.Html.Dsl                 ( HtmlCanvas )
import qualified Smart.Html.Dsl                as Dsl
import qualified Smart.Html.Render             as Render
import           Smart.Html.Shared.Html.Icons
import           Text.Blaze                     ( customAttribute )
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
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
    ! A.src "https://design.smart.coop/images/logo.svg"
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
                            ! A.id "i-understand"
                            ! A.name "i-understand"
                            ! A.type_ "checkbox"
                          H.div $ do
                            "I understand that this site is up for "
                            "demonstration purpose only, and that data are "
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
          H.a ! A.class_ "u-text-muted" ! A.href "#" $ "Log in"


--------------------------------------------------------------------------------
data ResultPage = Success Text
                | Failure Text

instance H.ToMarkup ResultPage where
  toMarkup = \case
    Success msg -> withText msg
    Failure msg -> withText msg
   where
    withText msg =
      Render.renderCanvas $ Dsl.SingletonCanvas $ H.code $ H.toMarkup msg
