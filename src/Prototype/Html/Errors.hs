{- |
Module: Prototype.Html.Errors
Description: Error pages.
-}
module Prototype.Html.Errors
  ( NotFoundPage(..)
  ) where

import qualified Smart.Html.Dsl                as Dsl
import qualified Smart.Html.Errors             as Errors
import qualified Smart.Html.Render             as Render
import qualified Text.Blaze.Html5              as H


--------------------------------------------------------------------------------
data NotFoundPage = NotFoundPage

instance H.ToMarkup NotFoundPage where
  toMarkup NotFoundPage = do
    Render.renderCanvas $ Dsl.SingletonCanvas Errors.NotFound
