{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{- |
Module: Curiosity.Data.Legal
Description: Legal entities related datatypes
-}
module Curiosity.Data.Legal
  ( Entity(..)
  , LegalId(..)
  , Err(..)
  ) where

import qualified Commence.Types.Wrapped        as W
import           Data.Aeson
import qualified Text.Blaze.Html5              as H
import           Web.FormUrlEncoded             ( FromForm(..) )


--------------------------------------------------------------------------------
data Entity = Entity
  { _entityId :: LegalId
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Record ID of the form LENT-xxx.
newtype LegalId = LegalId { unLegalId :: Text }
               deriving (Eq, Show)
               deriving ( IsString
                        , FromJSON
                        , ToJSON
                        , H.ToMarkup
                        , H.ToValue
                        ) via Text
               deriving FromForm via W.Wrapped "legal-id" Text

data Err = Err
  deriving (Eq, Exception, Show)
