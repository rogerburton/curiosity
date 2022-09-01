{- |
Module: Curiosity.Html.Business
Description: Business unit pages (view and edit).
-}
module Curiosity.Html.Business
  ( UnitView(..)
  , CreateUnitPage(..)
  ) where

import qualified Curiosity.Data.Business       as Business
import qualified Curiosity.Data.User           as User
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
  , _unitViewHasEditButton :: Maybe H.AttributeValue
  }

instance H.ToMarkup UnitView where
  toMarkup (UnitView unit hasEditButton) =
    renderView $ unitView unit hasEditButton

unitView unit hasEditButton = containerLarge $ do
  title' "Business unit" hasEditButton
  H.dl ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short" $ do
    keyValuePair "ID" (Business._entityId unit)


--------------------------------------------------------------------------------
data CreateUnitPage = CreateUnitPage
  { _createUnitPageUserProfile :: User.UserProfile
    -- ^ The user creating the unit
  , _createUnitPageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup CreateUnitPage where
  toMarkup (CreateUnitPage profile submitUrl) =
    renderForm profile "New business unit" $ do
      inputText "Unit name" "name" Nothing Nothing
      submitButton submitUrl "Create new business unit"
