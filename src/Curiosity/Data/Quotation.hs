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
  , QuotationState(..)
  , Predicate(..)
  , applyPredicate
  , Err(..)
  ) where

import qualified Commence.Types.Wrapped        as W
import qualified Curiosity.Data.User           as User
import           Data.Aeson
import qualified Text.Blaze.Html5              as H
import           Web.FormUrlEncoded             ( FromForm(..)
                                                , parseMaybe
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
  { _createQuotationClientUsername :: Maybe User.UserName
  }
  deriving (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance FromForm CreateQuotationAll where
  fromForm f = CreateQuotationAll <$> parseMaybe "client-username" f
    -- TODO Make it Nothing if empty string.


--------------------------------------------------------------------------------
-- $emptyValues
--
-- Since forms are designed to be submitted after a confirmation page, it
-- should be possible to re-display a form with pre-filled values. The initial
-- form, when no value has been provided by a user, is actually rendering
-- \"empty" values, defined here.

emptyCreateQuotationAll :: CreateQuotationAll
emptyCreateQuotationAll = CreateQuotationAll
  { _createQuotationClientUsername = Nothing
  }


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
  :: User.UserProfile -> CreateQuotationAll
  -> Maybe User.UserProfile -- ^ The user profile matching the quotation client.
  -> Either [Err] (Quotation, User.UserProfile)
validateCreateQuotation _ CreateQuotationAll {..} mresolvedClient = if null errors
  then Right (quotation, resolvedClient)
  else Left errors
 where
  quotation = Quotation
     { _quotationId = QuotationId "TODO-DUMMY"
     , _quotationState = QuotationSent
     }
  Just resolvedClient = mresolvedClient
  errors = concat
    [ if isJust _createQuotationClientUsername
      then []
      else [Err "Missing client username."]
    , if isJust mresolvedClient
      then []
      else [Err "The client username does not exist."]
    ]

-- | Similar to `validateCreateQuotation` but throw away the returned
-- contract, i.e. keep only the errors.
validateCreateQuotation' :: User.UserProfile -> CreateQuotationAll -> Maybe User.UserProfile -> [Err]
validateCreateQuotation' profile quotation resolvedClient =
  either identity (const []) $ validateCreateQuotation profile quotation resolvedClient


--------------------------------------------------------------------------------
-- | This represents a quotation in database.
data Quotation = Quotation
  { _quotationId    :: QuotationId
  , _quotationState :: QuotationState
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

data QuotationState = QuotationCreated | QuotationSent | QuotationSigned
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)


--------------------------------------------------------------------------------
-- | Predicates to filter quotations.
data Predicate = AllQuotations
  deriving (Eq, Show)

applyPredicate :: Predicate -> Quotation -> Bool
applyPredicate AllQuotations _ = True


--------------------------------------------------------------------------------
data Err = Err
  { unErr :: Text
  }
  deriving (Eq, Exception, Show)
