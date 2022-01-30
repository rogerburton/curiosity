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

import qualified Data.Text                     as T
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
dbUpdateParser = P.tryAlts [userCreate, userDelete, userUpdate]
 where
  userCreate =
    P.withTrailSpaces "UserCreate" *> fmap UserCreate userProfileParser
  userDelete = P.withTrailSpaces "UserDelete" *> userIdParser <&> UserDelete
  userUpdate =
    P.withTrailSpaces "UserUpdate" *> fmap UserUpdate userProfileParser

-- | For simplicity, we keep the parsers close to the actual data-constructors. 
dbSelectParser :: P.ParserText (Storage.DBSelect UserProfile)
dbSelectParser = P.tryAlts [userLogin, selectUserById]
 where
  userLogin =
    P.withTrailSpaces "UserLogin"
      *> (UserLogin <$> (userIdParser <* P.space1) <*> userPasswordParser)
  selectUserById =
    P.withTrailSpaces "SelectUserById" *> userIdParser <&> SelectUserById

-- | The UserId has to be non-empty alphaNumChar. 
userIdParser :: P.ParserText UserId
userIdParser = UserId . T.pack <$> P.many P.alphaNumChar

userNameParser :: P.ParserText UserName
userNameParser = UserName . T.pack <$> P.many P.alphaNumChar

-- | For the password, we just take the rest of the input.
userPasswordParser :: P.ParserText UserPassword
userPasswordParser = UserPassword . Secret <$> P.takeRest

userProfileParser :: P.ParserText UserProfile
userProfileParser =
  UserProfile
    <$> (userIdParser <* P.space1)
    <*> (userNameParser <* P.space1)
    <*> userPasswordParser
