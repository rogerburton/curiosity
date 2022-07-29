{- |
Module: Curiosity.Html.Errors
Description: Error pages.
-}
module Curiosity.Html.Errors
  ( NotFoundPage(..)
  , ErrorPage(..)
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


--------------------------------------------------------------------------------
data ErrorPage = ErrorPage Int Text Text

instance H.ToMarkup ErrorPage where
  toMarkup (ErrorPage code title subtitle) = do
    Render.renderCanvas . Dsl.SingletonCanvas $ Errors.Error code title subtitle
