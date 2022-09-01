{- |
Module: Curiosity.Html.Employment
Description: Employment contract pages (view and edit).
-}
module Curiosity.Html.Employment
  ( ContractView(..)
  , CreateContractPage(..)
  ) where

import qualified Curiosity.Data.Employment     as Employment
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
data ContractView = ContractView
  { _contractViewContract      :: Employment.Contract
  , _contractViewHasEditButton :: Maybe H.AttributeValue
  }

instance H.ToMarkup ContractView where
  toMarkup (ContractView contract hasEditButton) =
    renderView $ contractView contract hasEditButton

contractView contract hasEditButton = containerLarge $ do
  title' "Employment contract" hasEditButton
  H.dl ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short" $ do
    keyValuePair "ID" (Employment._contractId contract)


--------------------------------------------------------------------------------
data CreateContractPage = CreateContractPage
  { _createContractPageUserProfile :: User.UserProfile
    -- ^ The user creating the contract
  , _createContractPageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup CreateContractPage where
  toMarkup (CreateContractPage profile submitUrl) =
    renderForm profile "New employment contract" $ do
      inputText "Contract name" "name" Nothing Nothing
      submitButton submitUrl "Create new employment contract"
