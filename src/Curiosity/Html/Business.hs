{- |
Module: Curiosity.Html.Business
Description: Business unit pages (view and edit).
-}
module Curiosity.Html.Business
  ( UnitView(..)
  ) where

import qualified Curiosity.Data.Business       as Business
import           Curiosity.Html.Misc
import           Curiosity.Html.Navbar          ( navbar )
import qualified Smart.Html.Dsl                as Dsl
import qualified Smart.Html.Render             as Render
import           Smart.Html.Shared.Html.Icons   ( svgIconEdit )
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A


--------------------------------------------------------------------------------
data UnitView = UnitView
  { _unitViewUnit          :: Business.Entity
  , _unitViewHasEditButton :: Bool
  }

instance H.ToMarkup UnitView where
  toMarkup (UnitView unit hasEditButton) =
    renderView $ unitView unit hasEditButton

unitView unit hasEditButton = containerLarge $ do
  title' "Business unit" (Just "#")
  H.dl ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short" $ do
    keyValuePair "ID" (Business._entityId unit)
