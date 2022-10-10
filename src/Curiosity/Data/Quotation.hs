{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{- |
Module: Curiosity.Data.Quotation
Description: Quotation -related data types.

This module contains data types used to represent quotations, both as used when
filling a form, and used as proper validated data.

-}
module Curiosity.Data.Quotation
  ( -- * Form data representation
    --
    -- $formDataTypes
    CreateQuotationAll(..)
    -- * Empty values
    --
    -- $emptyValues
  , emptyCreateQuotationAll
    -- * Form submittal and validation
  , SubmitQuotation(..)
  , validateCreateQuotation
  , validateCreateQuotation'
    -- * Main data representation
  , Quotation(..)
  , QuotationId(..)
  , quotationIdPrefix
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
-- A quotation form, as displayed on a web page, is made of multiple input
-- groups (or panels, or even of separate pages). Different data types are
-- provided to represent those sets of input fields.

-- | This represents a form being filled in. In particular, it can represent
-- invalid inputs. As it is filled, it is kept in a Map in "Curiosity.Data",
-- where it is identified by a key. The form data are validated when they are
-- "submitted", using the `SubmitQuotation` data type below, and the key.
data CreateQuotationAll = CreateQuotationAll
  deriving (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance FromForm CreateQuotationAll where
  fromForm _ = pure CreateQuotationAll


--------------------------------------------------------------------------------
-- $emptyValues
--
-- Since forms are designed to be submitted after a confirmation page, it
-- should be possible to re-display a form with pre-filled values. The initial
-- form, when no value has been provided by a user, is actually rendering
-- \"empty" values, defined here.

emptyCreateQuotationAll :: CreateQuotationAll
emptyCreateQuotationAll = CreateQuotationAll


--------------------------------------------------------------------------------
-- | This represents the submittal of a CreateQuotationAll, identified by its
-- key.
data SubmitQuotation = SubmitQuotation
  { _submitQuotationKey :: Text
  }
  deriving (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance FromForm SubmitQuotation where
  fromForm f = SubmitQuotation <$> parseUnique "key" f

-- | Given a contract form, tries to return a proper `Quotation` value, although
-- the ID is dummy. Maybe we should have separate data types (with or without
-- the ID).
-- This is a pure function: everything required to perform the validation
-- should be provided as arguments.
validateCreateQuotation
  :: User.UserProfile -> CreateQuotationAll -> Either [Err] Quotation
validateCreateQuotation _ CreateQuotationAll = if null errors
  then Right quotation
  else Left errors
 where
  quotation = Quotation { _quotationId = QuotationId "TODO-DUMMY" }
  errors =
    concat
    -- No rules for now.
           []

-- | Similar to `validateCreateQuotation` but throw away the returned
-- contract, i.e. keep only the errors.
validateCreateQuotation' profile quotation =
  either identity (const []) $ validateCreateQuotation profile quotation


--------------------------------------------------------------------------------
-- | This represents a quotation in database.
data Quotation = Quotation
  { _quotationId :: QuotationId
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Record ID of the form QUOT-xxx.
newtype QuotationId = QuotationId { unQuotationId :: Text }
               deriving (Eq, Show)
               deriving ( IsString
                        , FromJSON
                        , ToJSON
                        , H.ToMarkup
                        , H.ToValue
                        ) via Text
               deriving FromForm via W.Wrapped "quotation-id" Text

quotationIdPrefix :: Text
quotationIdPrefix = "QUOT-"

data Err = Err
  { unErr :: Text
  }
  deriving (Eq, Exception, Show)
