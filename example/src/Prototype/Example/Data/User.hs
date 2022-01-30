{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{- |
Module: Prototype.Example.Data.User
Description: User related datatypes
-}
module Prototype.Example.Data.User
  ( UserProfile(..)
  , UserId(..)
  , UserName(..)
  , UserPassword(..)
  -- * Export all DB ops.
  , Storage.DBUpdate(..)
  , Storage.DBSelect(..)
  -- ** Parsers 
  , dbUpdateParser
  , dbSelectParser
  ) where

import qualified Prototype.Example.Repl.Parse  as P
import qualified Prototype.Runtime.Storage     as Storage
import           Prototype.Types.Secret        as Secret

data UserProfile = UserProfile
  { _userProfileId       :: UserId -- ^ Unique ID of the user, used for logging a user in.
  , _userProfileName     :: UserName -- ^ User's human friendly name.
  , _userProfilePassword :: UserPassword -- ^ User's password. 
  }
  deriving Show

newtype UserName = UserName Text
                 deriving (Eq, Show, IsString) via Text

newtype UserPassword = UserPassword (Secret.Secret '[] Text)
                 deriving (Eq, Show, IsString) via Text

newtype UserId = UserId Text
               deriving (Eq, Show)
               deriving IsString via Text

instance Storage.DBIdentity UserProfile where
  type DBId UserProfile = UserId
  dbId = _userProfileId

instance Storage.DBStorageOps UserProfile where
  data DBUpdate UserProfile =
    UserCreate UserProfile
    | UserDelete UserId
    | UserUpdate UserProfile
  
  data DBSelect UserProfile =
    UserLogin UserId UserPassword
    | SelectUserById UserId

dbUpdateParser :: P.ParserText (Storage.DBUpdate UserProfile)
dbUpdateParser = undefined

dbSelectParser :: P.ParserText (Storage.DBSelect UserProfile)
dbSelectParser = undefined
