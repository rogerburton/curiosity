{- |
Module: Curiosity.Html.LandingPage
Description: A landing page (when the user is not logge in) for Curiosity.
-}
module Curiosity.Html.LandingPage
  ( LandingPage(..)
  ) where

import qualified Smart.Html.Pages.LandingPage  as Pages
import qualified Smart.Html.Render             as Render
import qualified Text.Blaze.Html5              as H


--------------------------------------------------------------------------------
data LandingPage = LandingPage

instance H.ToMarkup LandingPage where
  toMarkup LandingPage = do
    Render.renderCanvas Pages.landingPage
