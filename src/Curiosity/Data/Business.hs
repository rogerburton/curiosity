{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{- |
Module: Curiosity.Data.Business
Description: Business entities related datatypes
-}
module Curiosity.Data.Business
  ( Entity(..)
  , BusinessId(..)
  , Err(..)
  ) where

import qualified Commence.Types.Wrapped        as W
import           Data.Aeson
import qualified Text.Blaze.Html5              as H
import           Web.FormUrlEncoded             ( FromForm(..) )


--------------------------------------------------------------------------------
data Entity = Entity
  { _entityId :: BusinessId
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Record ID of the form BENT-xxx.
newtype BusinessId = BusinessId { unBusinessId :: Text }
               deriving (Eq, Show)
               deriving ( IsString
                        , FromJSON
                        , ToJSON
                        , H.ToMarkup
                        , H.ToValue
                        ) via Text
               deriving FromForm via W.Wrapped "business-id" Text

data Err = Err
  deriving (Eq, Exception, Show)
