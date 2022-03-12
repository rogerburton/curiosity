{- |
Module: Prototype.Example.Server.Public.Pages
Description: Public pages for the application

The goal is to exemplify the use of our DSL for some simple pages and to have something more tangible to show.

-}
module Prototype.Example.Server.Public.Pages
  ( LoginPage(..)
  ) where

import qualified "start-servant" Prototype.Server.New.Page
                                               as P
import "design-hs-lib" Smart.Html.Render        ( renderCanvasWithHeadText )

import qualified "design-hs-lib" Smart.Html.Checkbox
                                               as C
import qualified "design-hs-lib" Smart.Html.Dsl
                                               as Dsl
import qualified "design-hs-lib" Smart.Html.Form
                                               as Form
import qualified "design-hs-lib" Smart.Html.Textarea
                                               as TA
import qualified Text.Blaze.Html5              as H

-- | A simple login page. 
newtype LoginPage  = LoginPage { _loginPageAuthSubmitURL :: H.AttributeValue }

-- | For the `LoginPage` markup, we now rely on our DSL to render the login page to our liking
instance H.ToMarkup LoginPage where
  toMarkup (LoginPage submitUrl) =
    H.toMarkup
      $ (Form.TextareaGroup [username, password] Dsl.::~ Dsl.SingletonCanvas
          (Form.CheckboxGroupInline "Remember me" [rememberMe]) :: Dsl.HtmlCanvas
        )
   where
    username   = ("Username", TA.Textarea 1 "id-username")
    password   = ("Password", TA.Textarea 1 "id-password")
    rememberMe = C.CheckboxEnabled
      (Just "id-remember-me")
      C.Unchecked
      "Check this to ensure your login is remembered on this browser."

