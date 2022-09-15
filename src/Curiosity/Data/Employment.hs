{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{- |
Module: Curiosity.Data.Employment
Description: Employment related datatypes
-}
module Curiosity.Data.Employment
  ( CreateContractAll(..)
  , CreateContractAll'(..)
  , CreateContractGenInfo(..)
  , CreateContractLocDates(..)
  , CreateContractRisks(..)
  , CreateContractInvoice(..)
  , AddExpense(..)
  , emptyCreateContractAll
  , emptyCreateContractGenInfo
  , emptyAddExpense
  , SubmitContract(..)
  , Contract(..)
  , ContractId(..)
  , Err(..)
  ) where

import qualified Commence.Types.Wrapped        as W
import           Data.Aeson
import qualified Text.Blaze.Html5              as H
import           Web.FormUrlEncoded             ( FromForm(..)
                                                , parseUnique
                                                )

--------------------------------------------------------------------------------
-- | This represent a form being filled in. In particular, it can represent
-- invalid inputs. As it is filled, it is kept in a Map, where it is identified
-- by a key. The form data are validated when they are "submitted", using the
-- SubmitContract data type below, and the key.
data CreateContractAll = CreateContractAll CreateContractGenInfo
                                           CreateContractLocDates
                                           CreateContractRisks
                                           CreateContractInvoice
                                           [AddExpense]
  deriving (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

-- | Same as above, but without the expenses. This is used to group together
-- the main panels into a `FromForm` instance. Simply leaving out the expenses
-- would also work but be less explicit.
data CreateContractAll' = CreateContractAll' CreateContractGenInfo
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

data CreateContractGenInfo = CreateContractGenInfo
  { _createContractProject     :: Text
  , _createContractPO          :: Text
  , _createContractRole        :: Text
  , _createContractType        :: Text
  , _createContractDescription :: Text
  }
  deriving (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance FromForm CreateContractGenInfo where
  fromForm f =
    CreateContractGenInfo
      <$> parseUnique "project"     f
      <*> parseUnique "po"          f
      <*> parseUnique "role"        f
      <*> parseUnique "type"        f
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

data AddExpense = AddExpense
  { _addExpenseAmount :: Int
  }
  deriving (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance FromForm AddExpense where
  fromForm f = AddExpense <$> parseUnique "amount" f

emptyCreateContractAll :: CreateContractAll
emptyCreateContractAll = CreateContractAll emptyCreateContractGenInfo
                                           emptyCreateContractLocDates
                                           emptyCreateContractRisks
                                           emptyCreateContractInvoice
                                           []

emptyCreateContractGenInfo :: CreateContractGenInfo
emptyCreateContractGenInfo = CreateContractGenInfo
  { _createContractProject     = ""
  , _createContractPO          = ""
  , _createContractRole        = ""
  , _createContractType        = ""
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

-- | This represents the submittal of a CreateContractAll, identified by its
-- key.
data SubmitContract = SubmitContract
  { _submitContractKey :: Text
  }
  deriving (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance FromForm SubmitContract where
  fromForm f = SubmitContract <$> parseUnique "key" f


--------------------------------------------------------------------------------
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

data Err = Err
  deriving (Eq, Exception, Show)
