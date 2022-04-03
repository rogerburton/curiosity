{- |
Module: Prototype.Example.Server.Public.Pages
Description: Public pages for the application

The goal is to exemplify the use of our DSL for some simple pages and to have something more tangible to show.

-}
module Prototype.Example.Server.Public.Pages
  ( LoginPage(..)
  , SignupPage(..)
  , SignupResultPage(..)
  ) where

import           Control.Lens
import qualified Data.Text                     as T
import qualified Prototype.Example.Data.User   as User
import qualified "design-hs-lib" Smart.Html.Button
                                               as Btn
import qualified "design-hs-lib" Smart.Html.Checkbox
                                               as C
import qualified "design-hs-lib" Smart.Html.Dsl
                                               as Dsl
import qualified "design-hs-lib" Smart.Html.Form
                                               as Form
import qualified "design-hs-lib" Smart.Html.Input
                                               as Inp
import qualified "design-hs-lib" Smart.Html.Shared.Types
                                               as HTypes
import qualified "design-hs-lib" Smart.Html.Textarea
                                               as TA
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import           Text.Blaze.Html5.Attributes

-- | A simple login page. 
newtype LoginPage  = LoginPage { _loginPageAuthSubmitURL :: H.AttributeValue }

-- | For the `LoginPage` markup, we now rely on our DSL to render the login page to our liking
instance H.ToMarkup LoginPage where
  toMarkup (LoginPage submitUrl) = do
    H.form
      . H.toMarkup @Dsl.HtmlCanvas
      $ (       Form.TextareaGroup [username, password]
        Dsl.::~ loginButton
        Dsl.::~ Dsl.SingletonCanvas
                  (Form.CheckboxGroupInline "Remember me" [rememberMe])
        )
   where
    username = ("Username", TA.Textarea 1 "id-username")
    password = ("Password", TA.Textarea 1 "id-password")
    rememberMe =
      C.CheckboxEnabled (Just "id-remember-me") C.Unchecked "Remember me."
    loginButton = mkButton "Login" submitUrl "POST"

mkButton text submitUrl method' =
  H.toMarkup (Btn.ButtonPrimary text HTypes.Enabled)
    ! formaction submitUrl
    ! formmethod method'

newtype SignupPage = SignupPage { _signupPageSubmitURL :: H.AttributeValue }

instance H.ToMarkup SignupPage where
  toMarkup (SignupPage submitUrl) = do
    H.form
      . H.toMarkup @Dsl.HtmlCanvas
      $ (       Form.InputGroup
            [username, password "Password", password "Confirm Password"]
        Dsl.::~ submitButton
        Dsl.::~ Dsl.EmptyCanvas
        )
   where
    username =
      ("Username", Inp.PlainTextInput HTypes.Enabled "id-username" Nothing)
    password fieldName =
      ( HTypes.Title fieldName
      , Inp.PasswordInput HTypes.Enabled
                          (HTypes.Id $ "id-" <> T.toLower fieldName)
                          Nothing
      )
    submitButton = mkButton "Register" submitUrl "POST"

data SignupResultPage = SignupSuccess User.UserId
                      | SignupFailed Text

instance H.ToMarkup SignupResultPage where
  toMarkup = \case
    SignupSuccess userId -> withText $ "Signed up as: " <> userId ^. coerced
    SignupFailed  msg    -> withText msg
   where
    withText msg =
      H.toMarkup @Dsl.HtmlCanvas $ Dsl.SingletonCanvas (HTypes.Title $ msg)

