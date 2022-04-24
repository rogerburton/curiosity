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
  , genRandomUserId
  , UserName(..)
  , Password(..)
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
import qualified Data.Text                     as T
import qualified Network.HTTP.Types            as HTTP
import qualified Prototype.Example.Repl.Parse  as P
import qualified "start-servant" Prototype.Lib.Wrapped
                                               as W
import qualified Prototype.Runtime.Errors      as Errs
import qualified Prototype.Runtime.Storage     as Storage
import qualified "start-servant" Prototype.Server.New.Page.Navbar
                                               as Nav
import qualified Prototype.Types.Secret        as Secret
import qualified Servant.Auth.Server           as SAuth
import qualified System.Random                 as Rand
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A
import           Web.FormUrlEncoded             ( FromForm(..) )
import           Web.HttpApiData                ( FromHttpApiData(..) )

-- | User's credentials. 
data UserCreds = UserCreds
  { _userCredsId       :: UserId
  , _userCredsPassword :: Password
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
                 deriving (FromHttpApiData, FromForm) via W.Wrapped "username" Text

newtype Password = Password (Secret.Secret '[ 'Secret.ToJSONExp] Text)
                 deriving (Eq, IsString) via Text
                 deriving ( FromHttpApiData
                          , FromJSON
                          , ToJSON
                          ) via (Secret.Secret '[ 'Secret.ToJSONExp] Text)
                 deriving stock Show
                 deriving FromForm via W.Wrapped "userPassword" Text

newtype UserId = UserId Text
               deriving (Eq, Show, SAuth.ToJWT, SAuth.FromJWT)
               deriving (IsString, FromHttpApiData, FromJSON, ToJSON) via Text
               deriving FromForm via W.Wrapped "userId" Text

-- | Randomly generated and character based user-id.
genRandomUserId :: forall m . MonadIO m => Int -> m UserId
genRandomUserId len =
  liftIO
    $   UserId
    .   T.pack
    .   take (abs len)
    .   Rand.randomRs ('a', 'z')
    <$> Rand.getStdGen

-- TODO
instance Nav.IsNavbarContent UserProfile where
  navbarMarkup UserProfile {..} = do
    greeting
    editProfileLink
    H.hr
   where
    greeting = H.div . H.text $ T.unwords ["Hi", _userProfileName ^. coerced]
    editProfileLink = H.a ! A.href "/private/user/profile" $ "Edit profile"

instance Storage.DBIdentity UserProfile where
  type DBId UserProfile = UserId
  dbId = _userCredsId . _userCreds

instance Storage.DBStorageOps UserProfile where
  data DBUpdate UserProfile =
    UserCreate UserProfile
    | UserCreateGeneratingUserId UserName Password
    | UserDelete UserId
    | UserUpdate UserProfile
    deriving (Show, Eq)
  
  data DBSelect UserProfile =
    UserLogin UserCreds
    | SelectUserById UserId
    deriving (Show, Eq)

dbUpdateParser :: P.ParserText (Storage.DBUpdate UserProfile)
dbUpdateParser = P.tryAlts
  [userCreate, userCreateGeneratingUserId, userDelete, userUpdate]
 where
  userCreate =
    P.withTrailSpaces "UserCreate" *> fmap UserCreate userProfileParser
  userCreateGeneratingUserId =
    P.withTrailSpaces "UserCreateGeneratingUserId"
      *> (UserCreateGeneratingUserId <$> userNameParser <*> userPasswordParser)
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
userPasswordParser :: P.ParserText Password
userPasswordParser = Password . Secret.Secret <$> P.punctuated
  (P.takeWhile1P (Just "Password") (not . Char.isPunctuation))

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
