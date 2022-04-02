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

import qualified Data.Text                     as T
import qualified "start-servant" Prototype.Server.New.Page
                                               as P
import "design-hs-lib" Smart.Html.Render        ( renderCanvasWithHeadText )

import qualified "design-hs-lib" Smart.Html.Button
                                               as Btn
import qualified "design-hs-lib" Smart.Html.Checkbox
                                               as C
import qualified "design-hs-lib" Smart.Html.Dsl
                                               as Dsl
import qualified "design-hs-lib" Smart.Html.Form
                                               as Form
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
    loginButton =
      H.toMarkup (Btn.ButtonPrimary "Login" HTypes.Enabled)
        ! formaction submitUrl
        ! formmethod "POST"

newtype SignupPage = SignupPage { _signupPageSubmitURL :: H.AttributeValue }

instance H.ToMarkup SignupPage where
  toMarkup (SignupPage submitUrl) = do
    H.form
      . H.toMarkup @Dsl.HtmlCanvas
      $ (       Form.TextareaGroup
            [username, password "Password", password "Confirm Password"]
        Dsl.::~ submitButton
        Dsl.::~ Dsl.EmptyCanvas
        )
   where
    username = ("Username", TA.Textarea 1 "id-username")
    password fieldName =
      ( HTypes.Title fieldName
      , TA.Textarea 1 . HTypes.Id $ "id-" <> T.toLower fieldName
      )
    submitButton =
      H.toMarkup (Btn.ButtonPrimary "Register" HTypes.Enabled)
        ! formaction submitUrl
        ! formmethod "POST"

data SignupResultPage = SignupSuccess
                      | SignupFailed Text

instance H.ToMarkup SignupResultPage where
  toMarkup = undefined
