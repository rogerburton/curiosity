{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{- |
Module: Curiosity.Data.Email
Description: Email -related data types.

This module contains data types used to represent emails. Within
`Curiosity.Data`, they are used to record atomically that an email should be
sent during an STM operation. A separate thread should actually handle them.

-}
module Curiosity.Data.Email
  ( -- * Main data representation
    Email(..)
  , EmailId(..)
  , emailIdPrefix
  , EmailTemplate(..)
  , emailTemplateName
  , Predicate(..)
  , applyPredicate
  , Err(..)
  ) where

import qualified Commence.Types.Wrapped        as W
import qualified Curiosity.Data.User           as User
import           Data.Aeson
import qualified Text.Blaze.Html5              as H
import           Web.FormUrlEncoded             ( FromForm(..)
                                                )


--------------------------------------------------------------------------------
-- | This represents an email in database.
data Email = Email
  { _emailId :: EmailId
  , _emailTemplate :: EmailTemplate
  , _emailRecipient :: User.UserEmailAddr
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Record ID of the form EMAIL-xxx.
newtype EmailId = EmailId { unEmailId :: Text }
               deriving (Eq, Show)
               deriving ( IsString
                        , FromJSON
                        , ToJSON
                        , H.ToMarkup
                        , H.ToValue
                        ) via Text
               deriving FromForm via W.Wrapped "email-id" Text

emailIdPrefix :: Text
emailIdPrefix = "EMAIL-"


--------------------------------------------------------------------------------
data EmailTemplate = SignupConfirmationEmail | QuotationEmail | InvoiceEmail | InvoiceReminderEmail
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

emailTemplateName :: EmailTemplate -> Text
emailTemplateName t = case t of
  SignupConfirmationEmail -> "SignupConfirmation"
  QuotationEmail -> "Quotation"
  InvoiceEmail -> "Invoice"
  InvoiceReminderEmail -> "InvoiceReminder"


--------------------------------------------------------------------------------
-- | Predicates to filter users.
data Predicate = AllEmails
  deriving (Eq, Show)

applyPredicate :: Predicate -> Email -> Bool
applyPredicate AllEmails _ = True


--------------------------------------------------------------------------------
data Err = Err
  { unErr :: Text
  }
  deriving (Eq, Exception, Show)
