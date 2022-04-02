{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{- |
Module: Prototype.Example.Data.User
Description: User related datatypes
-}
module Prototype.Example.Data.User
  ( UserCreds(..)
  , userCredsId
  , userCredsPassword
  , UserProfile(..)
  , userCreds
  , userProfileName
  , UserId(..)
  , UserName(..)
  , UserPassword(..)
  -- * Export all DB ops.
  , Storage.DBUpdate(..)
  , Storage.DBSelect(..)
  -- ** Parsers 
  , dbUpdateParser
  , dbSelectParser
  , userIdParser
  , userNameParser
  , userPasswordParser
  -- * Errors
  , UserErr(..)
  ) where

import           Control.Lens
import           Data.Aeson
import qualified Data.Char                     as Char
import qualified Network.HTTP.Types            as HTTP
import qualified Prototype.Example.Repl.Parse  as P
import qualified Prototype.Runtime.Errors      as Errs
import qualified Prototype.Runtime.Storage     as Storage
import           Prototype.Types.Secret        as Secret
import qualified Servant.Auth.Server           as SAuth
import           Web.FormUrlEncoded             ( Form(..)
                                                , FromForm(..)
                                                )
import           Web.HttpApiData                ( FromHttpApiData(..) )

-- | User's credentials. 
data UserCreds = UserCreds
  { _userCredsId       :: UserId
  , _userCredsPassword :: UserPassword
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, FromForm)

data UserProfile = UserProfile
  { _userCreds       :: UserCreds -- ^ Users credentials 
  , _userProfileName :: UserName -- ^ User's human friendly name.
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype UserName = UserName Text
                 deriving (Eq, Show, IsString, FromJSON , ToJSON) via Text

newtype UserPassword = UserPassword (Secret.Secret '[ 'Secret.ToJSONExp] Text)
                 deriving (Eq, IsString) via Text
                 deriving (FromHttpApiData, FromJSON, ToJSON) via (Secret.Secret '[ 'Secret.ToJSONExp] Text)
                 deriving stock Show

instance FromForm UserPassword where
  fromForm (Form hm) = case hm ^? ix "password" of
    Nothing    -> noneFound
    Just [pwd] -> Right $ pwd ^. coerced
    Just []    -> noneFound
    Just _     -> Left
      "Ambiguous list of passwords (too many passwords submitted in a form)."
    where noneFound = Left "No password found."

newtype UserId = UserId Text
               deriving (Eq, Show, SAuth.ToJWT)
               deriving (IsString, FromHttpApiData, FromJSON, ToJSON) via Text

instance Storage.DBIdentity UserProfile where
  type DBId UserProfile = UserId
  dbId = _userCredsId . _userCreds

instance Storage.DBStorageOps UserProfile where
  data DBUpdate UserProfile =
    UserCreate UserProfile
    | UserDelete UserId
    | UserUpdate UserProfile
    deriving (Show, Eq)
  
  data DBSelect UserProfile =
    UserLogin UserCreds
    | SelectUserById UserId
    deriving (Show, Eq)

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
dbSelectParser = userLogin <|> selectUserById
 where
  userLogin = P.withTrailSpaces "UserLogin" *> (UserLogin <$> userCredsParser)
  selectUserById =
    P.withTrailSpaces "SelectUserById" *> userIdParser <&> SelectUserById

-- | The UserId has to be non-empty ascii character 
userIdParser :: P.ParserText UserId
userIdParser = UserId <$> P.punctuated P.alphaNumText

userNameParser :: P.ParserText UserName
userNameParser = UserName <$> P.punctuated P.takeRest

-- | For the password, we want to consume everything within the punctuations. 
userPasswordParser :: P.ParserText UserPassword
userPasswordParser = UserPassword . Secret <$> P.punctuated
  (P.takeWhile1P (Just "UserPassword") (not . Char.isPunctuation))

userProfileParser :: P.ParserText UserProfile
userProfileParser =
  UserProfile <$> (userCredsParser <* P.space) <*> (userNameParser <* P.space)

userCredsParser =
  UserCreds <$> (userIdParser <* P.space) <*> userPasswordParser

data UserErr = UserExists Text
             | UserNotFound Text
             | IncorrectPassword Text
             deriving Show

instance Errs.IsRuntimeErr UserErr where
  errCode = errCode' . \case
    UserExists{}        -> "USER_EXISTS"
    UserNotFound{}      -> "USER_NOT_FOUND"
    IncorrectPassword{} -> "INCORRECT_PASSWORD"
    where errCode' = mappend "ERR.USER."

  httpStatus = \case
    UserExists{}        -> HTTP.conflict409
    UserNotFound{}      -> HTTP.notFound404
    IncorrectPassword{} -> HTTP.unauthorized401

  userMessage = Just . \case
    UserExists        msg -> msg
    UserNotFound      msg -> msg
    IncorrectPassword msg -> msg

makeLenses ''UserCreds
makeLenses ''UserProfile
