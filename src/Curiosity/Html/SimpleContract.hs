{-# LANGUAGE DataKinds #-}
{- |
Module: Curiosity.Html.SimpleContract
Description: Employment contract pages (view and edit) for the "simple" (also
called 3in1) contract flow.
-}
module Curiosity.Html.SimpleContract
  ( SimpleContractView(..)
  , CreateSimpleContractPage(..)
  , SelectRolePage(..)
  , ConfirmRolePage(..)
  --, AddExpensePage(..)
  --, RemoveExpensePage(..)
  , ConfirmSimpleContractPage(..)
  ) where

import qualified Curiosity.Data.SimpleContract as SimpleContract
import qualified Curiosity.Data.User           as User
import           Curiosity.Html.Misc
import qualified Smart.Html.Alert              as Alert
import qualified Smart.Html.Button             as Button
import           Smart.Html.Shared.Html.Icons
import qualified Smart.Html.Misc               as Misc
import           Smart.Html.Panel               ( Panel(..) )
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A


--------------------------------------------------------------------------------
data SimpleContractView = SimpleContractView
  { _contractViewSimpleContract      :: SimpleContract.Contract
  , _contractViewHasEditButton :: Maybe H.AttributeValue
  }

instance H.ToMarkup SimpleContractView where
  toMarkup (SimpleContractView contract hasEditButton) =
    renderView $ contractView contract hasEditButton

contractView contract hasEditButton = containerLarge $ do
  title' "Employment contract" hasEditButton
  H.dl ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short" $ do
    keyValuePair "ID" (SimpleContract._contractId contract)


--------------------------------------------------------------------------------
data CreateSimpleContractPage = CreateSimpleContractPage
  { _createSimpleContractPageUserProfile   :: User.UserProfile
    -- ^ The user creating the contract
  , _createSimpleContractPageKey           :: Maybe Text
    -- ^ The form editing session key, if the form was already saved
  , _createSimpleContractContract          :: SimpleContract.CreateContractAll
    -- ^ The contract being edited
  , _createSimpleContractPageSaveURL       :: H.AttributeValue
  , _createSimpleContractPageAddExpenseURL :: H.AttributeValue
  }

instance H.ToMarkup CreateSimpleContractPage where
  toMarkup (CreateSimpleContractPage profile mkey (SimpleContract.CreateContractAll SimpleContract.CreateContractType {..} SimpleContract.CreateContractLocDates{} SimpleContract.CreateContractRisks{} SimpleContract.CreateContractInvoice{} expenses) saveUrl addExpenseUrl)
    = renderFormLarge profile $ do
      autoReload
      title "New simple contract"

      let iconInformation = Just
            $ OSvgIconDiv @"circle-information" svgIconCircleInformation
      H.div ! A.class_ "u-spacer-bottom-l" $
        H.toMarkup $ Alert.Alert Alert.AlertDefault iconInformation "Message about the 3 simple contracts, possible without social shares." Button.NoButton

      (! A.id "panel-type") $ panel "Service type" $ do
        H.div ! A.class_ "o-form-group" $ do
          H.label ! A.class_ "o-form-group__label" ! A.for "worker" $ "Role"
          H.div ! A.class_ "o-form-group__controls o-form-group__controls--full-width" $ do
            H.div ! A.class_ "o-flex o-flex--vertical-center u-maximize-width" $ do
              H.span ! A.class_ "u-spacer-right-s" $
                "Art and craft creation > Entertainment arts > Dancer"
              H.button ! A.class_ "c-button c-button--borderless c-button--icon"
                       ! A.formaction "/echo/new-simple-contract-and-select-role"
                       ! A.formmethod "POST" $
                H.span ! A.class_ "c-button__content" $ do
                  divIconEdit
                  H.div ! A.class_ "u-sr-accessible" $ "Edit"
            H.p ! A.class_ "c-form-help-text" $
              "Your most frequently used role has been automatically selected. Click the edit button if you want to select a different role for this work."

        inputText "Role"
                  "role"
                  (Just $ H.toValue _createContractRole)
                  Nothing
        Misc.inputTextarea "description"
                           "Description"
                           6
                           "Describe your work (minimum 10 characters)"
                           _createContractDescription
                           True
      panel "Employment type" $ groupEmployment
      panel "Location and dates" $ do
        inputText "Work country" "country" Nothing Nothing
        inputText "Work dates"   "dates"   Nothing Nothing
      panel "Risks" $ groupRisks
      panel "Invoicing" $ groupInvoicing
      (! A.id "panel-expenses") $ panelStandard "Expenses" $ groupExpenses
        mkey
        expenses
        addExpenseUrl

      groupLayout $ do
        submitButton saveUrl "Save changes"
   where
    username =
      User.unUserName . User._userCredsName $ User._userProfileCreds profile

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

groupExpenses mkey expenses submitUrl = do
  H.div ! A.class_ "o-form-group" $ do
    when (null expenses)
      $ H.div
      ! A.class_ "c-blank-slate c-blank-slate--bg-alt"
      $ do
          H.p
            ! A.class_ "u-text-muted c-body-1"
            $ "When using a project, you can choose to add expenses directly or later."
          H.div ! A.class_ "c-button-toolbar" $ buttonAdd submitUrl
                                                          "Add expense"
    case mkey of
      Just key | not (null expenses) ->
        Misc.table titles (uncurry $ display key) $ zip [0 ..] expenses
      _ -> pure ()
    when (not $ null expenses) $ buttonGroup $ buttonAdd submitUrl "Add expense"
 where
  titles = ["Amount"]
  display
    :: Text
    -> Int
    -> SimpleContract.AddExpense
    -> ([Text], [(H.Html, Text, Text)], Maybe Text)
  display key i SimpleContract.AddExpense {..} =
    ( [show _addExpenseAmount]
    , [ ( Misc.divIconDelete
        , "Remove"
        , "/forms/remove-expense/" <> key <> "/" <> show i
        )
      ]
    , (Just $ "/forms/edit-expense/" <> key <> "/" <> show i)
    )

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
data SelectRolePage = SelectRolePage
  { _selectRolePageUserProfile :: User.UserProfile
    -- ^ The user creating the simple contract
  , _selectRolePageKey         :: Text
    -- ^ The key of the contract form
  , _selectRolePageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup SelectRolePage where
  toMarkup (SelectRolePage profile key submitUrl) =
    renderFormLarge profile $ do
      autoReload
      title' "Select role" Nothing

      H.div ! A.class_ "c-display" $ do
        H.h4 "Arts du spectacle"
        H.h4 "Arts littéraires"
        H.h4 "Arts plastiques et graphiques"
        H.ul $ do
          mapM_ (\(sym, label) -> H.li $ H.a ! A.href (H.toValue $ "/forms/confirm-role/" <> key <> "/" <> sym) $ H.text label)
            [ ("coloriste", "Coloriste")
            , ("dessinateur", "Dessinateur-rice / illustrateur-rice")
            , ("graffitiste", "Graffitiste / graffeur-euse")
            , ("graphiste", "Graphiste / infographiste / webdesigner-euse")
            , ("graveur", "Graveur-euse / sérigraphe")
            , ("peintre", "Peintre-esse")
            , ("performeur", "Performeur-euse")
            , ("photographe", "Photographe")
            , ("plasticien", "Plasticien-ne / installateur-rice 3d")
            , ("scenographe", "Scénographe")
            , ("sculpteur", "Sculpteur-rice")
            , ("body-painter", "Body-painter")
            , ("autre", "Autre")
            ]
        H.h4 "Architecture / mode / design / décoration"


--------------------------------------------------------------------------------
data ConfirmRolePage = ConfirmRolePage
  { _confirmRolePageUserProfile :: User.UserProfile
    -- ^ The user creating the simple contract
  , _confirmRolePageKey         :: Text
    -- ^ The key of the contract form
  , _confirmRolePageRole        :: Text
    -- ^ The role being selected
  , _confirmRolePageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup ConfirmRolePage where
  toMarkup (ConfirmRolePage profile key role submitUrl) =
    renderFormLarge profile $ do
      autoReload
      H.toMarkup $ PanelHeaderAndBody "Confirm selected role"
        $ H.dl
        ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short"
        $ do
            keyValuePair "Role" role

      H.input ! A.type_ "hidden" ! A.id "key" ! A.name "key" ! A.value
        (H.toValue key)

      buttonBar $ do
        buttonLink
          (H.toValue $ "/forms/edit-simple-contract/" <> key <> "#panel-type")
          "Cancel"
        buttonPrimary submitUrl "Select role"


--------------------------------------------------------------------------------
data AddExpensePage = AddExpensePage
  { _addExpensePageUserProfile :: User.UserProfile
    -- ^ The user creating the expense within a contract
  , _addExpensePageKey         :: Text
    -- ^ The key of the contract form
  , _addExpensePageIndex       :: Maybe Int
    -- ^ The index of the expense within CreateContractAll, if it was already
    -- added
  , _addExpensePageExpense     :: SimpleContract.AddExpense
  , _addExpensePageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup AddExpensePage where
  toMarkup (AddExpensePage profile key mindex expense submitUrl) =
    renderFormLarge profile $ do
      title' label Nothing

      panel "General information" $ do
        H.input
          ! A.type_ "hidden"
          ! A.id "contract-key"
          ! A.name "contract-key"
          ! A.value (H.toValue key)
        inputText "Amount" "amount" mamount Nothing

      H.input ! A.type_ "hidden" ! A.id "key" ! A.name "key" ! A.value
        (H.toValue key)
      buttonBar $ do
        buttonLink
          (H.toValue $ "/forms/edit-contract/" <> key <> "#panel-expenses")
          "Cancel"
        buttonPrimary submitUrl label
   where
    label   = if isJust mindex then "Update expense" else "Add expense"
    mamount = if amount /= 0 then Just (H.toValue amount) else Nothing
    amount  = SimpleContract._addExpenseAmount expense


--------------------------------------------------------------------------------
data RemoveExpensePage = RemoveExpensePage
  { _removeExpensePageUserProfile :: User.UserProfile
    -- ^ The user creating the expense within a contract
  , _removeExpensePageKey         :: Text
    -- ^ The key of the contract form
  , _removeExpensePageIndex       :: Int
    -- ^ The index of the expense within CreateContractAll
  , _removeExpensePageExpense     :: SimpleContract.AddExpense
  , _removeExpensePageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup RemoveExpensePage where
  toMarkup (RemoveExpensePage profile key mindex expense submitUrl) =
    renderFormLarge profile $ do
      title' "Remove expense" Nothing

      panel "General information" $ do
        keyValuePair "Amount" mamount

      button submitUrl "Remove expense"
   where
    mamount = if amount /= 0 then show amount else "/" :: Text
    amount  = SimpleContract._addExpenseAmount expense


--------------------------------------------------------------------------------
data ConfirmSimpleContractPage = ConfirmSimpleContractPage
  { _confirmSimpleContractPageUserProfile :: User.UserProfile
    -- ^ The user creating the contract
  , _confirmSimpleContractPageKey         :: Text
  , _confirmSimpleContractPageContract    :: SimpleContract.CreateContractAll
  , _confirmSimpleContractPageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup ConfirmSimpleContractPage where
  toMarkup (ConfirmSimpleContractPage profile key (SimpleContract.CreateContractAll SimpleContract.CreateContractType {..} SimpleContract.CreateContractLocDates{} SimpleContract.CreateContractRisks{} SimpleContract.CreateContractInvoice{} expenses) submitUrl)
    = renderFormLarge profile $ do
      title' "New employment contract"
        .  Just
        .  H.toValue
        $  "/forms/edit-contract/"
        <> key

      H.div
        ! A.class_ "u-padding-vertical-l"
        $ H.toMarkup
        $ PanelHeaderAndBody "General information"
        $ H.dl
        ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short"
        $ do
            keyValuePair "Worker username" username
            keyValuePair "Role"            _createContractRole
            keyValuePair "Description"     _createContractDescription

      H.div
        ! A.class_ "u-padding-vertical-l"
        $ H.toMarkup
        $ PanelHeaderAndBody "Expenses"
        $ H.dl
        ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short"
        $ if null expenses
            then H.div ! A.class_ "c-blank-slate c-blank-slate--bg-alt" $ do
              H.p
                ! A.class_ "u-text-muted c-body-1"
                $ "You have no expenses right now."
            else Misc.table titles (uncurry $ display key) $ zip [0 ..] expenses

      H.input ! A.type_ "hidden" ! A.id "key" ! A.name "key" ! A.value
        (H.toValue key)
      button submitUrl "Submit contract"
   where
    username =
      User.unUserName . User._userCredsName $ User._userProfileCreds profile
    titles = ["Amount"]
    display
      :: Text
      -> Int
      -> SimpleContract.AddExpense
      -> ([Text], [(H.Html, Text, Text)], Maybe Text)
    display key i SimpleContract.AddExpense {..} =
      ([show _addExpenseAmount], [], Nothing)
