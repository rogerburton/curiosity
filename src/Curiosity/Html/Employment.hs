{- |
Module: Curiosity.Html.Employment
Description: Employment contract pages (view and edit).
-}
module Curiosity.Html.Employment
  ( ContractView(..)
  , CreateContractPage(..)
  , AddExpensePage(..)
  , ConfirmContractPage(..)
  ) where

import qualified Curiosity.Data.Employment     as Employment
import qualified Curiosity.Data.User           as User
import           Curiosity.Html.Misc
import           Curiosity.Html.Navbar          ( navbar )
import qualified Smart.Html.Dsl                as Dsl
import qualified Smart.Html.Misc               as Misc
import           Smart.Html.Panel               ( Panel(..) )
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
  { _createContractPageUserProfile   :: User.UserProfile
    -- ^ The user creating the contract
  , _createContractContract          :: Employment.CreateContract
    -- ^ The contract being edited
  , _createContractPageSaveURL       :: H.AttributeValue
  , _createContractPageAddExpenseURL :: H.AttributeValue
  }

instance H.ToMarkup CreateContractPage where
  toMarkup (CreateContractPage profile Employment.CreateContract {..} saveUrl addExpenseUrl)
    = renderFormLarge profile $ do
      title "New employment contract"
      panel "General information" $ do

        -- TODO I guess this should be the real name ?
        formKeyValue "Worker name" displayName
        Misc.inputSelect_
          "project"
          "Project"
          [("none-selected", "Select a project"), ("a", "A")]
          (Just "Enter an existing project or create new one")
          (if _createContractProject == "none-selected"
            then Nothing
            else Just _createContractProject
          )
          True
        inputText "Purchase order"
                  "po"
                  (Just $ H.toValue _createContractPO)
                  Nothing
        inputText "Role" "role" (Just $ H.toValue _createContractRole) Nothing
        inputText "Work type"
                  "type"
                  (Just $ H.toValue _createContractType)
                  Nothing
        Misc.inputTextarea "description"
                           "Description"
                           6
                           "Describe your work (minimum 10 characters)"
                           _createContractDescription
                           True

      panel "Location and dates" $ do
        inputText "Work country" "country" Nothing Nothing
        inputText "Work dates"   "dates"   Nothing Nothing

      panel "Risks" $ groupRisks
      panel "Invoicing" $ groupInvoicing
      (! A.id "panel-expenses") $ panelStandard "Expenses" $ groupExpenses
        addExpenseUrl
      panel "Employment type" $ groupEmployment

      groupLayout $ do
        submitButton saveUrl "Save changes"

      autoReload
   where
    displayName = User.unUserDisplayName $ User._userProfileDisplayName profile

groupRisks = H.div ! A.class_ "o-form-group" $ do
  H.label ! A.class_ "o-form-group__label" $ "Work risks"
  H.div
    ! A.class_ "o-form-group__controls"
    $ H.div
    ! A.class_ "c-radio-group c-radio-group--inline"
    $ do
        H.div ! A.class_ "c-radio" $ H.label $ do
          H.input ! A.type_ "radio" ! A.name "radio1"
          "Without risks"
        H.div ! A.class_ "c-radio" $ H.label $ do
          H.input ! A.type_ "radio" ! A.name "radio1"
          "With risks"
        H.p ! A.class_ "c-form-help-text" $ do
          "For your safety and your colleagues safety, if your role "
          "involves risks, you have to select \"With risks\". "
          H.a ! A.class_ "c-link" ! A.href "#" $ "Learn more about risks."

groupInvoicing = do
  inputText "Client" "client" Nothing Nothing
  inputText "Amount" "amount" Nothing Nothing
  inputText "VAT"    "vat"    Nothing Nothing
  H.div ! A.class_ "o-form-group" $ do
    H.label ! A.class_ "o-form-group__label" $ "Include VAT ?"
    H.div
      ! A.class_ "o-form-group__controls"
      $ H.div
      ! A.class_ "c-radio-group c-radio-group--inline"
      $ do
          H.div ! A.class_ "c-radio" $ H.label $ do
            H.input ! A.type_ "radio" ! A.name "radio2"
            "Include"
          H.div ! A.class_ "c-radio" $ H.label $ do
            H.input ! A.type_ "radio" ! A.name "radio2"
            "Exclude"
          H.p
            ! A.class_ "c-form-help-text"
            $ "Usually, business clients prefer to see amounts VAT excluded."
  inputText "Down payment" "down-payment" Nothing Nothing

groupExpenses submitUrl = do
  H.div ! A.class_ "o-form-group" $ do
    H.label ! A.class_ "o-form-group__label" $ "Add expenses (optional)"
    H.div ! A.class_ "c-blank-slate c-blank-slate--bg-alt" $ do
      H.p
        ! A.class_ "u-text-muted c-body-1"
        $ "When using a project, you can choose to add expenses directly or later."
      H.div ! A.class_ "c-button-toolbar" $ buttonAdd submitUrl "Add expense"

groupEmployment = do
  inputText "Withholding tax"     "withholding-tax" Nothing Nothing
  inputText "Student / au cachet" "TODO"            Nothing Nothing
  inputText "Interim"             "interim"         Nothing Nothing

formKeyValue :: Text -> Text -> H.Html
formKeyValue label value = H.div ! A.class_ "o-form-group" $ do
  H.label ! A.class_ "o-form-group__label" $ H.text label
  H.div
    ! A.class_ "o-form-group__controls o-form-group__controls--full-width"
    $ H.label
    $ H.text value


--------------------------------------------------------------------------------
data AddExpensePage = AddExpensePage
  { _addExpensePageUserProfile :: User.UserProfile
    -- ^ The user creating the expense within a contract
  , _addExpensePageKey         :: Text
    -- ^ The key of the contract form
  , _addExpensePageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup AddExpensePage where
  toMarkup (AddExpensePage profile key submitUrl) =
    renderFormLarge profile $ do
      title' "Add expense" Nothing

      panel "General information" $ do
        H.input
          ! A.type_ "hidden"
          ! A.id "contract-key"
          ! A.name "contract-key"
          ! A.value (H.toValue key)
        inputText "Amount" "amount" Nothing Nothing

      H.input ! A.type_ "hidden" ! A.id "key" ! A.name "key" ! A.value
        (H.toValue key)
      button submitUrl "Add expense"
   where
    displayName = User.unUserDisplayName $ User._userProfileDisplayName profile


--------------------------------------------------------------------------------
data ConfirmContractPage = ConfirmContractPage
  { _confirmContractPageUserProfile :: User.UserProfile
    -- ^ The user creating the contract
  , _confirmContractPageKey         :: Text
  , _confirmContractPageContract    :: Employment.CreateContract
  , _confirmContractPageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup ConfirmContractPage where
  toMarkup (ConfirmContractPage profile key Employment.CreateContract {..} submitUrl)
    = renderFormLarge profile $ do
      title' "New employment contract"
        .  Just
        .  H.toValue
        $  "/forms/edit-contract/"
        <> key

      H.toMarkup
        $ PanelHeaderAndBody "General information"
        $ H.dl
        ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short"
        $ do
            keyValuePair "Worker name"    displayName
            keyValuePair "Project"        _createContractProject
            keyValuePair "Purchase order" _createContractPO
            keyValuePair "Role"           _createContractRole
            keyValuePair "Work type"      _createContractType
            keyValuePair "Description"    _createContractDescription

      H.input ! A.type_ "hidden" ! A.id "key" ! A.name "key" ! A.value
        (H.toValue key)
      button submitUrl "Submit contract"

      autoReload
   where
    displayName = User.unUserDisplayName $ User._userProfileDisplayName profile
