{- |
Module: Curiosity.Html.Homepage
Description: The homepage for Curiosity (when a user is logged in).
-}
module Curiosity.Html.Homepage
  ( WelcomePage(..)
  ) where

import           Curiosity.Html.Navbar          ( exampleNavbarAlt )
import qualified Smart.Html.Dsl                as Dsl
import qualified Smart.Html.Render             as Render
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A


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
