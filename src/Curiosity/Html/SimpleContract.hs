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
  , lookupRoleLabel
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
  , _createSimpleContractPageAddExpenseURL :: H.AttributeValue
  }

instance H.ToMarkup CreateSimpleContractPage where
  toMarkup (CreateSimpleContractPage profile mkey (SimpleContract.CreateContractAll SimpleContract.CreateContractType {..} SimpleContract.CreateContractLocDates{} SimpleContract.CreateContractRisks{} SimpleContract.CreateContractInvoice{} expenses) roleLabel saveUrl addExpenseUrl)
    = renderFormLarge profile $ do
      autoReload
      title "New simple contract"

      let iconInformation =
            Just $ OSvgIconDiv @"circle-information" svgIconCircleInformation
      H.div ! A.class_ "u-spacer-bottom-l" $ H.toMarkup $ Alert.Alert
        Alert.AlertDefault
        iconInformation
        "Message about the 3 simple contracts, possible without social shares."
        Button.NoButton

      (! A.id "panel-type") $ panel "Service type" $ do
        H.div ! A.class_ "o-form-group" $ do
          H.label ! A.class_ "o-form-group__label" ! A.for "worker" $ "Role"
          H.div
            ! A.class_
                "o-form-group__controls o-form-group__controls--full-width"
            $ do
                H.div
                  ! A.class_ "o-flex o-flex--vertical-center u-maximize-width"
                  $ do
                      H.span ! A.class_ "u-spacer-right-s" $ H.text roleLabel
                      H.button
                        ! A.class_
                            "c-button c-button--borderless c-button--icon"
                        ! A.formaction
                            "/echo/new-simple-contract-and-select-role"
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
          (H.toValue _createContractRole)
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
  }

instance H.ToMarkup SelectRolePage where
  toMarkup (SelectRolePage profile key) = renderFormLarge profile $ do
    autoReload
    title' "Select role" Nothing

    H.div ! A.class_ "c-display" $ do
      mapM_ (displayRole0 key) roles

displayRole0 key (Role0 title roles1) = do
  H.h3 $ H.text title
  mapM_ (displayRole1 key) roles1

displayRole1 key (Role1 title roles) = do
  H.h4 $ H.text title
  H.ul $ mapM_ (displayRole key) roles

displayRole key (value, label) =
  H.li
    $ H.a
    ! A.href (H.toValue $ "/forms/confirm-role/" <> key <> "/" <> value)
    $ H.text label

data Role0 = Role0 Text [Role1]

data Role1 = Role1 Text [Role]

type Role = (Text, Text)

roles :: [Role0]
roles =
  [ Role0
      "Fonction de création artistique et artisanale"
      [ Role1 "Arts du spectacle" []
      , Role1 "Arts littéraires"  []
      , Role1
        "Arts plastiques et graphiques"
        [ ("coloriste"   , "Coloriste")
        , ("dessinateur", "Dessinateur-rice / illustrateur-rice")
        , ("graffitiste" , "Graffitiste / graffeur-euse")
        , ("graphiste", "Graphiste / infographiste / webdesigner-euse")
        , ("graveur"     , "Graveur-euse / sérigraphe")
        , ("peintre"     , "Peintre-esse")
        , ("performeur"  , "Performeur-euse")
        , ("photographe" , "Photographe")
        , ("plasticien", "Plasticien-ne / installateur-rice 3d")
        , ("scenographe" , "Scénographe")
        , ("sculpteur"   , "Sculpteur-rice")
        , ("body-painter", "Body-painter")
        , ("autre"       , "Autre")
        ]
      , Role1 "Architecture / mode / design / décoration" []
      ]
  ]

-- Flatten the roles hierarchy, adding upper titles to the labels.
roles' :: [Role]
roles' = concatMap go0 roles
 where
  go0 (Role0 title0 roles1) = concatMap (go1 title0) roles1
  go1 title0 (Role1 title1 roles) = map (go title0 title1) roles
  go title0 title1 (value, label) =
    (value, unwords [title0, ">", title1, ">", label])

lookupRoleLabel role = lookup role roles'


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
  , _confirmRolePageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup ConfirmRolePage where
  toMarkup (ConfirmRolePage profile key role label submitUrl) =
    renderFormLarge profile $ do
      autoReload
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
  , _confirmSimpleContractPageRoleLabel   :: Text
    -- ^ The human text for the role, already looked up in `roles'`.
  , _confirmSimpleContractPageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup ConfirmSimpleContractPage where
  toMarkup (ConfirmSimpleContractPage profile key (SimpleContract.CreateContractAll SimpleContract.CreateContractType {..} SimpleContract.CreateContractLocDates{} SimpleContract.CreateContractRisks{} SimpleContract.CreateContractInvoice{} expenses) roleLabel submitUrl)
    = renderFormLarge profile $ do
      autoReload
      title' "New simple contract"
        .  Just
        .  H.toValue
        $  "/forms/edit-simple-contract/"
        <> key

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
