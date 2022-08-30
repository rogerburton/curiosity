{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{- |
Module: Curiosity.Data.Legal
Description: Legal entities related datatypes
-}
module Curiosity.Data.Legal
  ( Entity(..)
  , Create(..)
  , LegalId(..)
  , RegistrationName(..)
  , Err(..)
  , encodeUBL
  , toUBL
  ) where

import qualified Commence.Types.Wrapped        as W
import           Data.Aeson
import qualified Data.ByteString.Lazy          as LB
import qualified Text.Blaze.Html5              as H
import           Web.FormUrlEncoded             ( FromForm(..)
                                                , parseUnique
                                                )
import           Web.HttpApiData                ( FromHttpApiData(..) )


--------------------------------------------------------------------------------
data Create = Create
  { _createName :: RegistrationName
  }
  deriving (Generic, Eq, Show)

instance FromForm Create where
  fromForm f = Create <$> parseUnique "name" f


--------------------------------------------------------------------------------
data Entity = Entity
  { _entityId   :: LegalId
  , _entityName :: RegistrationName
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

-- | A registation name.
newtype RegistrationName = RegistrationName { unRegistrationName :: Text }
                 deriving ( Eq
                          , Show
                          , IsString
                          , FromJSON
                          , ToJSON
                          , H.ToMarkup
                          , H.ToValue
                          ) via Text
                 deriving (FromHttpApiData, FromForm) via W.Wrapped "name" Text

data Err = Err
  deriving (Eq, Exception, Show)


--------------------------------------------------------------------------------
encodeUBL :: Entity -> LB.ByteString
encodeUBL = encode . toUBL

toUBL :: Entity -> Value
toUBL Entity {..} =
  object ["CompanyID" .= _entityId, "RegistrationName" .= _entityName]
