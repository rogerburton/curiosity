{- |
Module: Prototype.Server.Public.Pages
Description: Public pages for the application

The goal is to exemplify the use of our DSL for some simple pages and to have something more tangible to show.

-}
module Prototype.Server.Public.Pages
  ( LandingPage(..)
  , NotFoundPage(..)
  ) where

import qualified Smart.Html.Dsl                as Dsl
import qualified Smart.Html.Errors             as Errors
import qualified Smart.Html.Pages.LandingPage  as Pages
import qualified Smart.Html.Render             as Render
import qualified Text.Blaze.Html5              as H

data LandingPage = LandingPage

instance H.ToMarkup LandingPage where
  toMarkup LandingPage = do
    Render.renderCanvas $ Pages.landingPage

data NotFoundPage = NotFoundPage

instance H.ToMarkup NotFoundPage where
  toMarkup NotFoundPage = do
    Render.renderCanvas $ Dsl.SingletonCanvas Errors.NotFound
