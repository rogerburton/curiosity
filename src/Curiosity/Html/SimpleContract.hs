{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
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
  , AddDatePage(..)
  , RemoveDatePage(..)
  , SelectVATPage(..)
  , ConfirmVATPage(..)
  , SimpleContractAddExpensePage(..)
  , SimpleContractRemoveExpensePage(..)
  , ConfirmSimpleContractPage(..)
  ) where

import qualified Curiosity.Data.SimpleContract as SimpleContract
import qualified Curiosity.Data.User           as User
import           Curiosity.Html.Misc
import           Data.List                      ( lookup )
import qualified Smart.Html.Alert              as Alert
import qualified Smart.Html.Button             as Button
import qualified Smart.Html.Misc               as Misc
import           Smart.Html.Panel               ( Panel(..) )
import           Smart.Html.Shared.Html.Icons
import           Smart.Html.Shared.Types        ( Body(..) )
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A


--------------------------------------------------------------------------------
data SimpleContractView = SimpleContractView
  { _contractViewSimpleContract :: SimpleContract.Contract
  , _contractViewHasEditButton  :: Maybe H.AttributeValue
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
  , _createSimpleContractRoleLabel         :: Text
    -- ^ The human text for the role, already looked un in `roles'`.
  , _createSimpleContractPageSaveURL       :: H.AttributeValue
  , _createSimpleContractPageSelectRoleURL :: H.AttributeValue
  , _createSimpleContractPageAddDateURL    :: H.AttributeValue
  , _createSimpleContractPageEditDateURL   :: Text
  , _createSimpleContractPageRemoveDateBaseURL :: Text
  , _createSimpleContractPageSelectVATURL  :: H.AttributeValue
  , _createSimpleContractPageAddExpenseURL :: H.AttributeValue
  , _createSimpleContractPageEditExpenseBaseURL :: Text
  , _createSimpleContractPageRemoveExpenseBaseURL :: Text
  }

instance H.ToMarkup CreateSimpleContractPage where
  toMarkup (CreateSimpleContractPage profile mkey contract roleLabel saveUrl selectRoleUrl addDateUrl editDateBaseUrl removeDateBaseUrl selectVATUrl addExpenseUrl editExpenseBaseUrl removeExpenseBaseUrl)
    = renderFormLarge profile $ do
      title "New simple contract"

      information

      (! A.id "panel-type") $ panel "Service type" $ groupType contract
                                                               roleLabel
                                                               selectRoleUrl
      panel "Risks" $ groupRisks
      (! A.id "panel-dates") $ panelStandard "Work dates" $ groupDates
        editDateBaseUrl
        removeDateBaseUrl
        mkey
        dates
        addDateUrl
      (! A.id "panel-client") $ panel "Client" $ groupClient client
      (! A.id "panel-invoicing") $ panel "Invoicing and contract type" $ groupInvoicing inv selectVATUrl
      (! A.id "panel-expenses") $ panelStandard "Expenses" $ groupExpenses
        editExpenseBaseUrl
        removeExpenseBaseUrl
        mkey
        expenses
        addExpenseUrl

      groupLayout $ do
        submitButton saveUrl "Save changes"
   where
    dates = SimpleContract._createContractDates contract
    client = SimpleContract._createContractClient contract
    inv = SimpleContract._createContractInvoice contract
    expenses = SimpleContract._createContractExpenses contract
    username =
      User.unUserName . User._userCredsName $ User._userProfileCreds profile

information =
  H.div ! A.class_ "u-spacer-bottom-l" $ H.toMarkup $ Alert.Alert
    Alert.AlertDefault
    iconInformation
    "Message about the 3 simple contracts, possible without social shares."
    Button.NoButton

iconInformation =
  Just $ OSvgIconDiv @"circle-information" svgIconCircleInformation

groupType SimpleContract.CreateContractAll {..} roleLabel selectRoleUrl = do
  H.div ! A.class_ "o-form-group" $ do
    H.label ! A.class_ "o-form-group__label" ! A.for "worker" $ "Role"
    H.div
      ! A.class_ "o-form-group__controls o-form-group__controls--full-width"
      $ do
          H.div
            ! A.class_ "o-flex o-flex--vertical-center u-maximize-width"
            $ do
                H.span ! A.class_ "u-spacer-right-s" $ H.text roleLabel
                H.button
                  ! A.class_ "c-button c-button--borderless c-button--icon"
                  ! A.formaction selectRoleUrl
                  ! A.formmethod "POST"
                  $ H.span
                  ! A.class_ "c-button__content"
                  $ do
                      divIconEdit
                      H.div ! A.class_ "u-sr-accessible" $ "Edit"
          H.p
            ! A.class_ "c-form-help-text"
            $ "Your most frequently used role has been automatically selected. Click the edit button if you want to select a different role for this work."

  H.input ! A.type_ "hidden" ! A.id "role" ! A.name "role" ! A.value
    (H.toValue $ SimpleContract._createContractRole _createContractType)
  Misc.inputTextarea
    "description"
    "Description"
    6
    "A description of your work. This will appear on the invoice sent to the client. (Minimum 10 characters.)"
    (SimpleContract._createContractDescription _createContractType)
    True
  Misc.inputSelect_
    "work-country"
    "Work country"
    countries
    Nothing
    (Just $ SimpleContract._createContractWorkCountry _createContractType)
    False
  inputRisks $ SimpleContract._createContractHasRisks _createContractType

-- TODO Same in Employment.Contract.
inputRisks hasRisks = H.div ! A.class_ "o-form-group" $ do
  H.label ! A.class_ "o-form-group__label" $ "Work risks"
  H.div
    ! A.class_ "o-form-group__controls"
    $ H.div
    ! A.class_ "c-radio-group c-radio-group--inline"
    $ do
        H.div ! A.class_ "c-radio" $ H.label $ do
          (if not hasRisks then (! A.checked "checked") else identity) $
            H.input ! A.type_ "radio" ! A.name "has-risks" ! A.value "False"
          "Without risks"
        H.div ! A.class_ "c-radio" $ H.label $ do
          (if hasRisks then (! A.checked "checked") else identity) $
            H.input ! A.type_ "radio" ! A.name "has-risks" ! A.value "True"
          "With risks"
        H.p ! A.class_ "c-form-help-text" $ do
          "For your safety and your colleagues safety, if your role "
          "involves risks, you have to select \"With risks\". "
          H.a ! A.class_ "c-link" ! A.href "#" $ "Learn more about risks."

groupRisks = H.div ! A.class_ "o-form-group" $ do
  H.toMarkup $ Alert.AlertLight
    Alert.AlertDefault
    iconInformation
    "Fill this panel only when you selected \"With risks\" above."
    Button.NoButton

-- TODO This is the same editing pattern as groupExpenses: start with an empty
-- list, then add / delete / edit items. This should be abstracted away.
groupDates editDateBaseUrl removeDateBaseUrl mkey dates submitUrl = do
  H.div ! A.class_ "o-form-group" $ do
    when (null dates)
      $ H.div
      ! A.class_ "c-blank-slate c-blank-slate--bg-alt"
      $ do
          H.p
            ! A.class_ "u-text-muted c-body-1"
            $ "Select the dates when you'll be working. All the dates must be within the same month."
          H.div ! A.class_ "c-button-toolbar" $ buttonAdd submitUrl
                                                          "Add date"
    case mkey of
      Just key | not (null dates) ->
        Misc.table titles (uncurry $ display key) $ zip [0 ..] dates
      _ -> pure ()
    when (not $ null dates) $ buttonGroup $ buttonAdd submitUrl "Add date"
 where
  titles = ["Date"]
  display
    :: Text
    -> Int
    -> SimpleContract.AddDate
    -> ([Text], [(H.Html, Text, Text)], Maybe Text)
  display key i SimpleContract.AddDate {..} =
    ( [_addDateDate]
    , [ ( Misc.divIconDelete
        , "Remove"
        , removeDateBaseUrl <> key <> "/" <> show i
        )
      ]
    , (Just $ editDateBaseUrl <> key <> "/" <> show i)
    )

groupClient SimpleContract.CreateContractClient {..} = do
  inputText "Client" "client-username" musername (Just "The client username.")
 where
  musername = (H.toValue . User.unUserName) <$> _createContractClientUsername

groupInvoicing inv selectVATUrl = do
  let vat = SimpleContract._createContractVAT inv
      amount = SimpleContract._createContractAmount inv
      prepaid = SimpleContract._createContractPrepaidAmount inv
      whtax = SimpleContract._createContractWithholdingTax inv
  H.div ! A.class_ "o-form-group" $ do
    H.label ! A.class_ "o-form-group__label" $
      "Amount"
    H.div ! A.class_ "o-form-group__controls o-form-group__controls--full-width" $ do
      H.div ! A.class_ "o-flex-bp2 o-flex--spaced-wide o-flex--vertical-center" $ do
        H.div ! A.class_ "c-input-group" $ do
          H.div ! A.class_ "c-input-group__input" $
            H.input ! A.class_ "c-input" ! A.type_ "number" ! A.id "amount" ! A.name "amount" ! A.value (H.toValue amount)
          H.div ! A.class_ "c-input-group__append" $ "€"
        H.div ! A.class_ "c-checkbox" $
          H.label $ do
            H.input ! A.type_ "checkbox"
            "Unknown amount"
      H.p ! A.class_ "c-form-help-text" $
        "Enter an amount or select \"Unknown amount\"."
  H.div ! A.class_ "o-form-group" $ do
    H.label ! A.class_ "o-form-group__label" $
      "VAT rate"
    H.div ! A.class_ "o-form-group__controls o-form-group__controls--full-width" $ do
      H.div ! A.class_ "o-flex-bp2 o-flex--spaced-wide o-flex--vertical-center" $ do
        H.div ! A.class_ "c-input-group" $ do
          H.div ! A.class_ "c-input-group__input" $
            H.input ! A.class_ "c-input" ! A.id "vat" ! A.name "vat" ! A.value (H.toValue vat)
          H.div ! A.class_ "c-input-group__append" $ "%"
        buttonSecondary selectVATUrl "Change VAT"
      H.p ! A.class_ "c-form-help-text" $
        "The normal VAT is 21%. In some cases a reduced rate can be applied using the \"Change VAT rate\" button."
  H.div ! A.class_ "o-form-group" $ do
    H.label ! A.class_ "o-form-group__label" $ "Include VAT ?"
    H.div
      ! A.class_ "o-form-group__controls"
      $ H.div
      ! A.class_ "c-radio-group c-radio-group--inline"
      $ do
          H.div ! A.class_ "c-radio" $ H.label $ do
            H.input ! A.type_ "radio" ! A.name "vat-incl-excl" ! A.value "True"
            "VAT Included"
          H.div ! A.class_ "c-radio" $ H.label $ do
            H.input ! A.type_ "radio" ! A.name "vat-incl-excl" ! A.value "False"
            "VAT Excluded"
          H.p
            ! A.class_ "c-form-help-text"
            $ "Specify if the above amount includes VAT or not."
  inputText "Prepaid amount" "prepaid-amount" (Just $ H.toValue prepaid) Nothing
  inputText "Withholding tax"     "withholding-tax" (Just $ H.toValue whtax) Nothing
  Misc.inputSelect_
    "contract-type"
    "Contract type"
    [ ("none-selected", "Please select a contract type")
    , ("normal", "Duration-based work contract")
    , ("artistic", "Task-based artistic contract")
    , ("artistic-1bis", "1st bis artistic contract")
    , ("student", "Student work contract")
    , ("interim", "Interim contract")
    ]
    Nothing
    Nothing
    False

groupExpenses editExpenseBaseUrl removeExpenseBaseUrl mkey expenses submitUrl = do
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
        , removeExpenseBaseUrl <> key <> "/" <> show i
        )
      ]
    , (Just $ editExpenseBaseUrl <> key <> "/" <> show i)
    )

formKeyValue :: Text -> Text -> H.Html
formKeyValue label value = H.div ! A.class_ "o-form-group" $ do
  H.label ! A.class_ "o-form-group__label" $ H.text label
  H.div
    ! A.class_ "o-form-group__controls o-form-group__controls--full-width"
    $ H.label
    $ H.text value


--------------------------------------------------------------------------------
data SelectRolePage = SelectRolePage
  { _selectRolePageUserProfile        :: User.UserProfile
    -- ^ The user creating the simple contract
  , _selectRolePageKey                :: Text
    -- ^ The key of the contract form
  , _selectRolePageConfirmRoleBaseURL :: H.AttributeValue
  }

instance H.ToMarkup SelectRolePage where
  toMarkup (SelectRolePage profile key confirmRoleBaseUrl) = renderFormLarge profile $ do
    title' "Select role" Nothing

    H.div ! A.class_ "c-display" $ do
      mapM_ (displayRole0 confirmRoleBaseUrl key) SimpleContract.roles

displayRole0 confirmRoleBaseUrl key (SimpleContract.Role0 title roles1) = do
  H.h3 $ H.text title
  mapM_ (displayRole1 confirmRoleBaseUrl key) roles1

displayRole1 confirmRoleBaseUrl key (SimpleContract.Role1 title roles) = do
  H.h4 $ H.text title
  H.ul $ mapM_ (displayRole confirmRoleBaseUrl key) roles

displayRole confirmRoleBaseUrl key (value, label) =
  H.li
    $ H.a
    ! A.href (confirmRoleBaseUrl <> H.toValue (key <> "/" <> value))
    $ H.text label


--------------------------------------------------------------------------------
data ConfirmRolePage = ConfirmRolePage
  { _confirmRolePageUserProfile :: User.UserProfile
    -- ^ The user creating the simple contract
  , _confirmRolePageKey         :: Text
    -- ^ The key of the contract form
  , _confirmRolePageRole        :: Text
    -- ^ The role being selected
  , _confirmRolePageLabel       :: Text
    -- ^ The human text for the role, already looked up in `roles'`.
  , _confirmRolePageEditURL     :: H.AttributeValue
  , _confirmRolePageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup ConfirmRolePage where
  toMarkup (ConfirmRolePage profile key role label editUrl submitUrl) =
    renderFormLarge profile $ do
      H.toMarkup
        $ PanelHeaderAndBody "Confirm selected role"
        $ H.dl
        ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short"
        $ do
            keyValuePair "Role" label
            H.input ! A.type_ "hidden" ! A.id "role" ! A.name "role" ! A.value
              (H.toValue role)

      H.input ! A.type_ "hidden" ! A.id "key" ! A.name "key" ! A.value
        (H.toValue key)

      buttonBar $ do
        buttonLink editUrl "Cancel"
        buttonPrimary submitUrl "Select role"


--------------------------------------------------------------------------------
data AddDatePage = AddDatePage
  { _addDatePageUserProfile :: User.UserProfile
    -- ^ The user creating the date within a contract
  , _addDatePageKey         :: Text
    -- ^ The key of the contract form
  , _addDatePageIndex       :: Maybe Int
    -- ^ The index of the date within CreateContractAll, if it was already
    -- added
  , _addDatePageDate        :: SimpleContract.AddDate
  , _addDatePageEditURL     :: H.AttributeValue
  , _addDatePageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup AddDatePage where
  toMarkup (AddDatePage profile key mindex date editUrl submitUrl) =
    renderFormLarge profile $ do
      title' label Nothing

      panel "Work dates" $ do
        inputText "Date" "date" mamount Nothing

      H.input ! A.type_ "hidden" ! A.id "key" ! A.name "key" ! A.value
        (H.toValue key)
      buttonBar $ do
        buttonLink editUrl "Cancel"
        buttonPrimary submitUrl label
   where
    label   = if isJust mindex then "Update date" else "Add date"
    mamount = if amount /= "1970-01-01" then Just (H.toValue amount) else Nothing
    amount  = SimpleContract._addDateDate date


--------------------------------------------------------------------------------
data RemoveDatePage = RemoveDatePage
  { _removeDatePageUserProfile :: User.UserProfile
    -- ^ The user creating the date within a contract
  , _removeDatePageKey         :: Text
    -- ^ The key of the contract form
  , _removeDatePageIndex       :: Int
    -- ^ The index of the date within CreateContractAll
  , _removeDatePageDate     :: SimpleContract.AddDate
  , _removeDatePageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup RemoveDatePage where
  toMarkup (RemoveDatePage profile key mindex date submitUrl) =
    renderFormLarge profile $ do
      title' "Remove date" Nothing

      panel "Work dates" $ do
        keyValuePair "Date" mamount

      button submitUrl "Remove date"
   where
    mamount = if amount /= "1970-01-01" then amount else "/" :: Text
    amount  = SimpleContract._addDateDate date


--------------------------------------------------------------------------------
data SelectVATPage = SelectVATPage
  { _selectVATPageUserProfile       :: User.UserProfile
    -- ^ The user creating the simple contract
  , _selectVATPageKey               :: Text
    -- ^ The key of the contract form
  , _selectVATPageConfirmVatBaseUrl :: H.AttributeValue
  }

instance H.ToMarkup SelectVATPage where
  toMarkup (SelectVATPage profile key confirmVatBaseUrl) = renderFormLarge profile $ do
    title' "Select VAT" Nothing

    displayRate confirmVatBaseUrl key ("0", "0% For some reason")
    displayRate confirmVatBaseUrl key ("6", "6% For some reason")
    displayRate confirmVatBaseUrl key ("21", "21% For some reason")

displayRate confirmVatBaseUrl key (value, label) =
  H.li
    $ H.a
    ! A.href (confirmVatBaseUrl <> H.toValue ( key <> "/" <> value))
    $ H.text label

--------------------------------------------------------------------------------
data ConfirmVATPage = ConfirmVATPage
  { _confirmVATPageUserProfile :: User.UserProfile
    -- ^ The user creating the simple contract
  , _confirmVATPageKey         :: Text
    -- ^ The key of the contract form
  , _confirmVATPageRate        :: Int
    -- ^ The rate being selected
  , _confirmVATPageEditURL     :: H.AttributeValue
  , _confirmVATPageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup ConfirmVATPage where
  toMarkup (ConfirmVATPage profile key rate editUrl submitUrl) =
    renderFormLarge profile $ do
      H.toMarkup
        $ PanelHeaderAndBody "Confirm selected VAT"
        $ H.dl
        ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short"
        $ do
            keyValuePair "VAT" (show rate <> "%" :: Text)
            H.input ! A.type_ "hidden" ! A.id "vat" ! A.name "vat" ! A.value
              (H.toValue rate)

      H.input ! A.type_ "hidden" ! A.id "key" ! A.name "key" ! A.value
        (H.toValue key)

      buttonBar $ do
        buttonLink editUrl "Cancel"
        buttonPrimary submitUrl "Select VAT"


--------------------------------------------------------------------------------
data SimpleContractAddExpensePage = SimpleContractAddExpensePage
  { _addExpensePageUserProfile :: User.UserProfile
    -- ^ The user creating the expense within a contract
  , _addExpensePageKey         :: Text
    -- ^ The key of the contract form
  , _addExpensePageIndex       :: Maybe Int
    -- ^ The index of the expense within CreateContractAll, if it was already
    -- added
  , _addExpensePageExpense     :: SimpleContract.AddExpense
  , _addExpensePageEditURL     :: H.AttributeValue
  , _addExpensePageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup SimpleContractAddExpensePage where
  toMarkup (SimpleContractAddExpensePage profile key mindex expense editUrl submitUrl) =
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
        buttonLink editUrl "Cancel"
        buttonPrimary submitUrl label
   where
    label   = if isJust mindex then "Update expense" else "Add expense"
    mamount = if amount /= 0 then Just (H.toValue amount) else Nothing
    amount  = SimpleContract._addExpenseAmount expense


--------------------------------------------------------------------------------
data SimpleContractRemoveExpensePage = SimpleContractRemoveExpensePage
  { _removeExpensePageUserProfile :: User.UserProfile
    -- ^ The user creating the expense within a contract
  , _removeExpensePageKey         :: Text
    -- ^ The key of the contract form
  , _removeExpensePageIndex       :: Int
    -- ^ The index of the expense within CreateContractAll
  , _removeExpensePageExpense     :: SimpleContract.AddExpense
  , _removeExpensePageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup SimpleContractRemoveExpensePage where
  toMarkup (SimpleContractRemoveExpensePage profile key mindex expense submitUrl) =
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
  , _confirmSimpleContractPageErrors      :: [SimpleContract.Err]
    -- Validation errors, if any.
  , _confirmSimpleContractPageRoleLabel   :: Text
    -- ^ The human text for the role, already looked up in `roles'`.
  , _confirmSimpleContractPageEditURL     :: Maybe H.AttributeValue
  , _confirmSimpleContractPageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup ConfirmSimpleContractPage where
  toMarkup (ConfirmSimpleContractPage profile key (SimpleContract.CreateContractAll SimpleContract.CreateContractType {..} SimpleContract.CreateContractRisks{} SimpleContract.CreateContractClient {..} SimpleContract.CreateContractInvoice{} dates expenses) errors roleLabel meditUrl submitUrl)
    = renderFormLarge profile $ do
      title' "New simple contract" meditUrl

      validationErrors errors

      H.div
        ! A.class_ "u-padding-vertical-l"
        $ H.toMarkup
        $ PanelHeaderAndBody "Service type"
        $ H.dl
        ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short"
        $ do
            keyValuePair "Worker username" username
            keyValuePair "Role"            roleLabel
            keyValuePair "Description"     _createContractDescription
            keyValuePair "Work country" $ maybe "TODO" identity $ lookupCountry
              _createContractWorkCountry
            keyValuePair "Work risks"     $ if _createContractHasRisks then "With risks" else "Without risks" :: Text

      H.div
        ! A.class_ "u-padding-vertical-l"
        $ H.toMarkup
        $ PanelHeaderAndBody "Risks"
        $ H.dl
        ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short"
        $ H.div ! A.class_ "c-blank-slate c-blank-slate--bg-alt" $ do
            H.p
              ! A.class_ "u-text-muted c-body-1"
              $ ""

      H.div
        ! A.class_ "u-padding-vertical-l"
        $ H.toMarkup
        $ PanelHeaderAndBody "Work dates"
        $ H.dl
        ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short"
        $ if null dates
            then H.div ! A.class_ "c-blank-slate c-blank-slate--bg-alt" $ do
              H.p
                ! A.class_ "u-text-muted c-body-1"
                $ "At least one date is required."
            else H.ul $
                   mapM_ (H.li . H.text . SimpleContract._addDateDate) dates

      H.div
        ! A.class_ "u-padding-vertical-l"
        $ H.toMarkup
        $ PanelHeaderAndBody "Client"
        $ H.dl
        ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short"
        $ case mClientUsername of
            Nothing ->
              H.div ! A.class_ "c-blank-slate c-blank-slate--bg-alt" $ do
                H.p
                  ! A.class_ "u-text-muted c-body-1"
                  $ "A client must be selected."
            Just clientUsername ->
              keyValuePair "Client username" clientUsername

      H.div
        ! A.class_ "u-padding-vertical-l"
        $ H.toMarkup
        $ PanelHeaderAndBody "Invoicing and contract type"
        $ H.dl
        ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short"
        $ "TODO"

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
                $ "You have added no expenses."
            else Misc.table titles (uncurry $ display key) $ zip [0 ..] expenses

      H.input ! A.type_ "hidden" ! A.id "key" ! A.name "key" ! A.value
        (H.toValue key)
      button submitUrl "Submit contract" -- TODO Add edit button
   where
    username =
      User.unUserName . User._userCredsName $ User._userProfileCreds profile
    mClientUsername = User.unUserName <$> _createContractClientUsername
    titles = ["Amount"]
    display
      :: Text
      -> Int
      -> SimpleContract.AddExpense
      -> ([Text], [(H.Html, Text, Text)], Maybe Text)
    display key i SimpleContract.AddExpense {..} =
      ([show _addExpenseAmount], [], Nothing)

validationErrors errors =
  if null errors
  then mempty
  else
    H.div ! A.class_ "u-spacer-bottom-l" $ H.toMarkup $ Alert.Alert
      Alert.AlertError
      iconError
      (Body $ "Validation errors: " <> show (map SimpleContract.unErr errors))
      Button.NoButton

iconError =
  Just $ OSvgIconDiv @"circle-error" svgIconCircleError


--------------------------------------------------------------------------------
-- | From https://github.com/noteed/start-web/blob/main/data/countries.sql.
-- Originally from pycountry.
countries :: [(Text, Text)]
countries =
  [ ("AW", "Aruba")
  , ("AF", "Afghanistan")
  , ("AO", "Angola")
  , ("AI", "Anguilla")
  , ("AX", "Åland Islands")
  , ("AL", "Albania")
  , ("AD", "Andorra")
  , ("AE", "United Arab Emirates")
  , ("AR", "Argentina")
  , ("AM", "Armenia")
  , ("AS", "American Samoa")
  , ("AQ", "Antarctica")
  , ("TF", "French Southern Territories")
  , ("AG", "Antigua and Barbuda")
  , ("AU", "Australia")
  , ("AT", "Austria")
  , ("AZ", "Azerbaijan")
  , ("BI", "Burundi")
  , ("BE", "Belgium")
  , ("BJ", "Benin")
  , ("BQ", "Bonaire, Sint Eustatius and Saba")
  , ("BF", "Burkina Faso")
  , ("BD", "Bangladesh")
  , ("BG", "Bulgaria")
  , ("BH", "Bahrain")
  , ("BS", "Bahamas")
  , ("BA", "Bosnia and Herzegovina")
  , ("BL", "Saint Barthélemy")
  , ("BY", "Belarus")
  , ("BZ", "Belize")
  , ("BM", "Bermuda")
  , ("BO", "Bolivia, Plurinational State of")
  , ("BR", "Brazil")
  , ("BB", "Barbados")
  , ("BN", "Brunei Darussalam")
  , ("BT", "Bhutan")
  , ("BV", "Bouvet Island")
  , ("BW", "Botswana")
  , ("CF", "Central African Republic")
  , ("CA", "Canada")
  , ("CC", "Cocos , (Keeling) Islands")
  , ("CH", "Switzerland")
  , ("CL", "Chile")
  , ("CN", "China")
  , ("CI", "Côte d'Ivoire")
  , ("CM", "Cameroon")
  , ("CD", "Congo, The Democratic Republic of the")
  , ("CG", "Congo")
  , ("CK", "Cook Islands")
  , ("CO", "Colombia")
  , ("KM", "Comoros")
  , ("CV", "Cabo Verde")
  , ("CR", "Costa Rica")
  , ("CU", "Cuba")
  , ("CW", "Curaçao")
  , ("CX", "Christmas Island")
  , ("KY", "Cayman Islands")
  , ("CY", "Cyprus")
  , ("CZ", "Czechia")
  , ("DE", "Germany")
  , ("DJ", "Djibouti")
  , ("DM", "Dominica")
  , ("DK", "Denmark")
  , ("DO", "Dominican Republic")
  , ("DZ", "Algeria")
  , ("EC", "Ecuador")
  , ("EG", "Egypt")
  , ("ER", "Eritrea")
  , ("EH", "Western Sahara")
  , ("ES", "Spain")
  , ("EE", "Estonia")
  , ("ET", "Ethiopia")
  , ("FI", "Finland")
  , ("FJ", "Fiji")
  , ("FK", "Falkland Islands , (Malvinas)")
  , ("FR", "France")
  , ("FO", "Faroe Islands")
  , ("FM", "Micronesia, Federated States of")
  , ("GA", "Gabon")
  , ("GB", "United Kingdom")
  , ("GE", "Georgia")
  , ("GG", "Guernsey")
  , ("GH", "Ghana")
  , ("GI", "Gibraltar")
  , ("GN", "Guinea")
  , ("GP", "Guadeloupe")
  , ("GM", "Gambia")
  , ("GW", "Guinea-Bissau")
  , ("GQ", "Equatorial Guinea")
  , ("GR", "Greece")
  , ("GD", "Grenada")
  , ("GL", "Greenland")
  , ("GT", "Guatemala")
  , ("GF", "French Guiana")
  , ("GU", "Guam")
  , ("GY", "Guyana")
  , ("HK", "Hong Kong")
  , ("HM", "Heard Island and McDonald Islands")
  , ("HN", "Honduras")
  , ("HR", "Croatia")
  , ("HT", "Haiti")
  , ("HU", "Hungary")
  , ("ID", "Indonesia")
  , ("IM", "Isle of Man")
  , ("IN", "India")
  , ("IO", "British Indian Ocean Territory")
  , ("IE", "Ireland")
  , ("IR", "Iran, Islamic Republic of")
  , ("IQ", "Iraq")
  , ("IS", "Iceland")
  , ("IL", "Israel")
  , ("IT", "Italy")
  , ("JM", "Jamaica")
  , ("JE", "Jersey")
  , ("JO", "Jordan")
  , ("JP", "Japan")
  , ("KZ", "Kazakhstan")
  , ("KE", "Kenya")
  , ("KG", "Kyrgyzstan")
  , ("KH", "Cambodia")
  , ("KI", "Kiribati")
  , ("KN", "Saint Kitts and Nevis")
  , ("KR", "Korea, Republic of")
  , ("KW", "Kuwait")
  , ("LA", "Lao People's Democratic Republic")
  , ("LB", "Lebanon")
  , ("LR", "Liberia")
  , ("LY", "Libya")
  , ("LC", "Saint Lucia")
  , ("LI", "Liechtenstein")
  , ("LK", "Sri Lanka")
  , ("LS", "Lesotho")
  , ("LT", "Lithuania")
  , ("LU", "Luxembourg")
  , ("LV", "Latvia")
  , ("MO", "Macao")
  , ("MF", "Saint Martin , (French part)")
  , ("MA", "Morocco")
  , ("MC", "Monaco")
  , ("MD", "Moldova, Republic of")
  , ("MG", "Madagascar")
  , ("MV", "Maldives")
  , ("MX", "Mexico")
  , ("MH", "Marshall Islands")
  , ("MK", "North Macedonia")
  , ("ML", "Mali")
  , ("MT", "Malta")
  , ("MM", "Myanmar")
  , ("ME", "Montenegro")
  , ("MN", "Mongolia")
  , ("MP", "Northern Mariana Islands")
  , ("MZ", "Mozambique")
  , ("MR", "Mauritania")
  , ("MS", "Montserrat")
  , ("MQ", "Martinique")
  , ("MU", "Mauritius")
  , ("MW", "Malawi")
  , ("MY", "Malaysia")
  , ("YT", "Mayotte")
  , ("NA", "Namibia")
  , ("NC", "New Caledonia")
  , ("NE", "Niger")
  , ("NF", "Norfolk Island")
  , ("NG", "Nigeria")
  , ("NI", "Nicaragua")
  , ("NU", "Niue")
  , ("NL", "Netherlands")
  , ("NO", "Norway")
  , ("NP", "Nepal")
  , ("NR", "Nauru")
  , ("NZ", "New Zealand")
  , ("OM", "Oman")
  , ("PK", "Pakistan")
  , ("PA", "Panama")
  , ("PN", "Pitcairn")
  , ("PE", "Peru")
  , ("PH", "Philippines")
  , ("PW", "Palau")
  , ("PG", "Papua New Guinea")
  , ("PL", "Poland")
  , ("PR", "Puerto Rico")
  , ("KP", "Korea, Democratic People's Republic of")
  , ("PT", "Portugal")
  , ("PY", "Paraguay")
  , ("PS", "Palestine, State of")
  , ("PF", "French Polynesia")
  , ("QA", "Qatar")
  , ("RE", "Réunion")
  , ("RO", "Romania")
  , ("RU", "Russian Federation")
  , ("RW", "Rwanda")
  , ("SA", "Saudi Arabia")
  , ("SD", "Sudan")
  , ("SN", "Senegal")
  , ("SG", "Singapore")
  , ("GS", "South Georgia and the South Sandwich Islands")
  , ("SH", "Saint Helena, Ascension and Tristan da Cunha")
  , ("SJ", "Svalbard and Jan Mayen")
  , ("SB", "Solomon Islands")
  , ("SL", "Sierra Leone")
  , ("SV", "El Salvador")
  , ("SM", "San Marino")
  , ("SO", "Somalia")
  , ("PM", "Saint Pierre and Miquelon")
  , ("RS", "Serbia")
  , ("SS", "South Sudan")
  , ("ST", "Sao Tome and Principe")
  , ("SR", "Suriname")
  , ("SK", "Slovakia")
  , ("SI", "Slovenia")
  , ("SE", "Sweden")
  , ("SZ", "Eswatini")
  , ("SX", "Sint Maarten , (Dutch part)")
  , ("SC", "Seychelles")
  , ("SY", "Syrian Arab Republic")
  , ("TC", "Turks and Caicos Islands")
  , ("TD", "Chad")
  , ("TG", "Togo")
  , ("TH", "Thailand")
  , ("TJ", "Tajikistan")
  , ("TK", "Tokelau")
  , ("TM", "Turkmenistan")
  , ("TL", "Timor-Leste")
  , ("TO", "Tonga")
  , ("TT", "Trinidad and Tobago")
  , ("TN", "Tunisia")
  , ("TR", "Turkey")
  , ("TV", "Tuvalu")
  , ("TW", "Taiwan, Province of China")
  , ("TZ", "Tanzania, United Republic of")
  , ("UG", "Uganda")
  , ("UA", "Ukraine")
  , ("UM", "United States Minor Outlying Islands")
  , ("UY", "Uruguay")
  , ("US", "United States")
  , ("UZ", "Uzbekistan")
  , ("VA", "Holy See , (Vatican City State)")
  , ("VC", "Saint Vincent and the Grenadines")
  , ("VE", "Venezuela, Bolivarian Republic of")
  , ("VG", "Virgin Islands, British")
  , ("VI", "Virgin Islands, U.S.")
  , ("VN", "Viet Nam")
  , ("VU", "Vanuatu")
  , ("WF", "Wallis and Futuna")
  , ("WS", "Samoa")
  , ("YE", "Yemen")
  , ("ZA", "South Africa")
  , ("ZM", "Zambia")
  , ("ZW", "Zimbabwe")
  ]

lookupCountry code = lookup code countries
