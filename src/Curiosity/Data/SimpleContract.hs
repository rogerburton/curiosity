{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{- |
Module: Curiosity.Data.SimpleContract
Description: Simple contract (also called 3in1) related datatypes

This module contains data types used to represent simple contracts, both as
used when filling a form, and used as proper validated data.

See also the [related documentation page](/documentation/objects/invoices).

-}
module Curiosity.Data.SimpleContract
  ( -- * Form data representation
    --
    -- $formDataTypes
    CreateContractAll(..)
  , CreateContractAll'(..)
  , CreateContractType(..)
  , CreateContractLocDates(..)
  , CreateContractRisks(..)
  , CreateContractInvoice(..)
  , SelectRole(..)
  , AddExpense(..)
    -- * Empty values
    --
    -- $emptyValues
  , emptyCreateContractAll
  , emptyCreateContractType
  , emptyCreateContractLocDates
  , emptyCreateContractRisks
  , emptyCreateContractInvoice
  , emptyAddExpense
    -- * Form submittal
  , SubmitContract(..)
  , validateCreateContract
    -- * Main data representation
  , Contract(..)
  , ContractId(..)
  , Err(..)
  ) where

import qualified Commence.Types.Wrapped        as W
import qualified Curiosity.Data.User           as User
import           Data.Aeson
import qualified Text.Blaze.Html5              as H
import           Web.FormUrlEncoded             ( FromForm(..)
                                                , parseUnique
                                                )

--------------------------------------------------------------------------------
-- $formDataTypes
--
-- A contract form, as displayed on a web page, is made of multiple input
-- groups (or panels, or even of separate pages). Different data types are
-- provided to represent those sets of input fields.

-- | This represents a form being filled in. In particular, it can represent
-- invalid inputs. As it is filled, it is kept in a Map in "Curiosity.Data",
-- where it is identified by a key. The form data are validated when they are
-- "submitted", using the `SubmitContract` data type below, and the key.
data CreateContractAll = CreateContractAll CreateContractType
                                           CreateContractLocDates
                                           CreateContractRisks
                                           CreateContractInvoice
                                           [AddExpense]
  deriving (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

-- | Same as above, but without the expenses. This is used to group together
-- the main panels into a `FromForm` instance. Simply leaving out the expenses
-- would also work but be less explicit.
data CreateContractAll' = CreateContractAll' CreateContractType
                                             CreateContractLocDates
                                             CreateContractRisks
                                             CreateContractInvoice
  deriving (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance FromForm CreateContractAll' where
  fromForm f =
    CreateContractAll'
      <$> fromForm f
      <*> fromForm f
      <*> fromForm f
      <*> fromForm f

data CreateContractType = CreateContractType
  { _createContractRole        :: Text
  , _createContractDescription :: Text
  }
  deriving (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance FromForm CreateContractType where
  fromForm f =
    CreateContractType
      <$> parseUnique "role"        f
      <*> parseUnique "description" f

data CreateContractLocDates = CreateContractLocDates
  deriving (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance FromForm CreateContractLocDates where
  fromForm f = pure CreateContractLocDates

data CreateContractRisks = CreateContractRisks
  deriving (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance FromForm CreateContractRisks where
  fromForm f = pure CreateContractRisks

data CreateContractInvoice = CreateContractInvoice
  deriving (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance FromForm CreateContractInvoice where
  fromForm f = pure CreateContractInvoice

data SelectRole = SelectRole
  { _selectRoleRole :: Text
  }
  deriving (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance FromForm SelectRole where
  fromForm f = SelectRole <$> parseUnique "role" f

data AddExpense = AddExpense
  { _addExpenseAmount :: Int
  }
  deriving (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance FromForm AddExpense where
  fromForm f = AddExpense <$> parseUnique "amount" f


--------------------------------------------------------------------------------
-- $emptyValues
--
-- Since forms are designed to be submitted after a confirmation page, it
-- should be possible to re-display a form with pre-filled values. The initial
-- form, when no value has been provided by a user, is actually rendering
-- \"empty" values, defined here.

emptyCreateContractAll :: CreateContractAll
emptyCreateContractAll = CreateContractAll emptyCreateContractType
                                           emptyCreateContractLocDates
                                           emptyCreateContractRisks
                                           emptyCreateContractInvoice
                                           []

emptyCreateContractType :: CreateContractType
emptyCreateContractType = CreateContractType
  { _createContractRole        = ""
  , _createContractDescription = ""
  }

emptyCreateContractLocDates :: CreateContractLocDates
emptyCreateContractLocDates = CreateContractLocDates

emptyCreateContractRisks :: CreateContractRisks
emptyCreateContractRisks = CreateContractRisks

emptyCreateContractInvoice :: CreateContractInvoice
emptyCreateContractInvoice = CreateContractInvoice

emptyAddExpense :: AddExpense
emptyAddExpense = AddExpense { _addExpenseAmount = 0 }


--------------------------------------------------------------------------------
-- | This represents the submittal of a CreateContractAll, identified by its
-- key.
data SubmitContract = SubmitContract
  { _submitContractKey :: Text
  }
  deriving (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance FromForm SubmitContract where
  fromForm f = SubmitContract <$> parseUnique "key" f

-- | Given a contract form, tries to return a proper `Contract` value, although
-- the ID is dummy. Maybe we should have separate data types (with or without
-- the ID).
-- This is a pure function: everything required to perform the validation
-- should be provided as arguments.
validateCreateContract
  :: User.UserProfile -> CreateContractAll -> Either Err Contract
validateCreateContract profile = do
  if User.CanCreateContracts `elem` User._userProfileRights profile
    then pure $ Right Contract { _contractId = ContractId "TODO-DUMMY" }
    else pure . Left $ Err "User has not the right CanCreateContracts."


--------------------------------------------------------------------------------
-- | This represents a contract in database. TODO The notion of contract
-- includes more than amployment contract and all should share most of their
-- structure.
data Contract = Contract
  { _contractId :: ContractId
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Record ID of the form EMP-xxx.
newtype ContractId = ContractId { unContractId :: Text }
               deriving (Eq, Show)
               deriving ( IsString
                        , FromJSON
                        , ToJSON
                        , H.ToMarkup
                        , H.ToValue
                        ) via Text
               deriving FromForm via W.Wrapped "contract-id" Text

data Err = Err Text
  deriving (Eq, Exception, Show)
