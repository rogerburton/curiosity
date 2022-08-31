{- |
Module: Curiosity.Html.Legal
Description: Legal entity pages (view and edit).
-}
module Curiosity.Html.Legal
  ( EntityView(..)
  , CreateEntityPage(..)
  ) where

import qualified Curiosity.Data.Legal          as Legal
import qualified Curiosity.Data.User           as User
import           Curiosity.Html.Misc
import           Curiosity.Html.Navbar          ( navbar )
import qualified Smart.Html.Dsl                as Dsl
import qualified Smart.Html.Render             as Render
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A


--------------------------------------------------------------------------------
data EntityView = EntityView
  { _entityViewEntity        :: Legal.Entity
  , _entityViewHasEditButton :: Bool
  }

instance H.ToMarkup EntityView where
  toMarkup (EntityView entity hasEditButton) =
    renderView $ entityView entity hasEditButton

entityView entity hasEditButton = containerLarge $ do
  title' "Legal entity" (Just "#")
  H.dl ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short" $ do
    keyValuePair "ID"                (Legal._entityId entity)
    keyValuePair "Registration name" (Legal._entityName entity)
    keyValuePair "CBE number"        (Legal._entityCbeNumber entity)
    keyValuePair "VAT number"        (Legal._entityVatNumber entity)


--------------------------------------------------------------------------------
data CreateEntityPage = CreateEntityPage
  { _createEntityPageUserProfile :: User.UserProfile
    -- ^ The user creating the entity
  , _createEntityPageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup CreateEntityPage where
  toMarkup (CreateEntityPage profile submitUrl) =
    renderForm profile "New legal entity" $ do
      inputText "Registration name" "name" Nothing Nothing
      inputText "CBE number" "cbe-number" Nothing $ Just "Example: 100200300"
      inputText "VAT number" "vat-number" Nothing $ Just "Example: BE0100200300"
      submitButton submitUrl "Create new legal entity"
