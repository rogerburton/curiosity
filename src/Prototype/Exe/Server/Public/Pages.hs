{- |
Module: Prototype.Exe.Server.Public.Pages
Description: Public pages for the application

The goal is to exemplify the use of our DSL for some simple pages and to have something more tangible to show.

-}
module Prototype.Exe.Server.Public.Pages
  ( SignupResultPage(..)
  , LandingPage(..)
  , NotFoundPage(..)
  ) where

import           Control.Lens
import qualified Prototype.Exe.Data.User       as User
import qualified Smart.Html.Dsl                as Dsl
import qualified Smart.Html.Errors             as Errors
import qualified Smart.Html.Pages.LandingPage  as Pages
import qualified Smart.Html.Render             as Render
import qualified Smart.Html.Shared.Types       as HTypes
import qualified Text.Blaze.Html5              as H

data SignupResultPage = SignupSuccess User.UserId
                      | SignupFailed Text

instance H.ToMarkup SignupResultPage where
  toMarkup = \case
    SignupSuccess userId -> withText $ "Signed up as: " <> userId ^. coerced
    SignupFailed  msg    -> withText $ "Failed sign up: " <> msg
   where
    withText msg =
      H.toMarkup @Dsl.HtmlCanvas $ Dsl.SingletonCanvas (HTypes.Title $ msg)

data LandingPage = LandingPage

instance H.ToMarkup LandingPage where
  toMarkup LandingPage = do
    Render.renderCanvas $
      Pages.landingPage

data NotFoundPage = NotFoundPage

instance H.ToMarkup NotFoundPage where
  toMarkup NotFoundPage = do
    Render.renderCanvas $
      Dsl.SingletonCanvas Errors.NotFound
