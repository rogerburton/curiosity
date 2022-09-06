{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{- |
Module: Curiosity.Data.Employment
Description: Employment related datatypes
-}
module Curiosity.Data.Employment
  ( CreateContract(..)
  , emptyCreateContract
  , AddExpense(..)
  , AddExpenseId(..)
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
import           Web.HttpApiData                ( FromHttpApiData(..) )

--------------------------------------------------------------------------------
-- | This represent a form being filled in. In particular, it can represent
-- invalid inputs. As it is filled, it is kept in a Map, where it is identified
-- by a key. The form data are validated when they are "submitted", using the
-- SubmitContract data type below, and the key.
data CreateContract = CreateContract
  { _createContractProject     :: Text
  , _createContractPO          :: Text
  , _createContractRole        :: Text
  , _createContractType        :: Text
  , _createContractDescription :: Text
  }
  deriving (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance FromForm CreateContract where
  fromForm f =
    CreateContract
      <$> parseUnique "project"     f
      <*> parseUnique "po"          f
      <*> parseUnique "role"        f
      <*> parseUnique "type"        f
      <*> parseUnique "description" f

emptyCreateContract :: CreateContract
emptyCreateContract = CreateContract { _createContractProject     = ""
                                     , _createContractPO          = ""
                                     , _createContractRole        = ""
                                     , _createContractType        = ""
                                     , _createContractDescription = ""
                                     }

data AddExpense = AddExpense
  { _addExpenseCreateContract :: Text
  , _addExpenseAmount         :: Int
  }
  deriving (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance FromForm AddExpense where
  fromForm f =
    AddExpense <$> parseUnique "contract-key" f <*> parseUnique "amount" f

newtype AddExpenseId = AddExpenseId Text
  deriving (Eq, Ord, Show)
  deriving ( IsString
           , FromJSON
           , ToJSON
           , H.ToMarkup
           , H.ToValue
           ) via Text
  deriving (FromHttpApiData, FromForm) via W.Wrapped "user-id" Text

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
