{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{- |
Module: Curiosity.Data.Business
Description: Business entities related datatypes
-}
module Curiosity.Data.Business
  ( Unit(..)
  , Create(..)
  , Update(..)
  , UnitId(..)
  , unitIdPrefix
  , Err(..)
  ) where

import qualified Commence.Types.Wrapped        as W
import           Data.Aeson
import qualified Text.Blaze.Html5              as H
import           Web.FormUrlEncoded             ( FromForm(..)
                                                , parseMaybe
                                                , parseUnique
                                                )


--------------------------------------------------------------------------------
data Create = Create
  { _createSlug :: Text -- Unique
  , _createName :: Text
  }
  deriving (Generic, Eq, Show)

instance FromForm Create where
  fromForm f = Create <$> parseUnique "slug" f <*> parseUnique "name" f

-- | Represents the input data to update a business unit profile.
data Update = Update
  { _updateSlug        :: Text
  , _updateDescription :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromForm Update where
  fromForm f = Update <$> parseUnique "slug" f <*> parseMaybe "description" f


--------------------------------------------------------------------------------
data Unit = Unit
  { _entityId          :: UnitId
  , _entitySlug        :: Text
    -- An identifier suitable for URLs
  , _entityName        :: Text
  , _entityDescription :: Maybe Text
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Record ID of the form BENT-xxx.
newtype UnitId = UnitId { unUnitId :: Text }
               deriving (Eq, Show)
               deriving ( IsString
                        , FromJSON
                        , ToJSON
                        , H.ToMarkup
                        , H.ToValue
                        ) via Text
               deriving FromForm via W.Wrapped "unit-id" Text

unitIdPrefix :: Text
unitIdPrefix = "BENT-"

data Err = Err
  { unErr :: Text
  }
  deriving (Eq, Exception, Show)
