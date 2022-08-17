{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{- |
Module: Curiosity.Data.User
Description: User related datatypes
-}
module Curiosity.Data.User
  ( Signup(..)
  , Login(..)
  , Credentials(..)
  , Update(..)
  , userCredsName
  , userCredsPassword
  , UserProfile'(..)
  , UserProfile
  , UserCompletion1(..)
  , UserCompletion2(..)
  , AccessRight(..)
  , userProfileCreds
  , userProfileId
  , userProfileDisplayName
  , userProfileEmailAddr
  , userProfileTosConsent
  , userProfileAddrAndTelVerified
  , userProfileEId
  , userProfileEIdVerified
  , userProfileEmailAddrVerified
  , userProfilePostalAddress
  , userProfileTelephoneNbr
  , UserId(..)
  , UserName(..)
  , Password(..)
  , Predicate(..)
  , applyPredicate
  -- * Export all DB ops.
  , Storage.DBUpdate(..)
  , Storage.DBSelect(..)
  -- * Errors
  , UserErr(..)
  , usernameBlocklist
  ) where

import qualified Commence.Runtime.Errors       as Errs
import qualified Commence.Runtime.Storage      as Storage
import qualified Commence.Types.Secret         as Secret
import qualified Commence.Types.Wrapped        as W
import           Control.Lens
import qualified Curiosity.Html.Errors         as Pages
import           Data.Aeson
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import qualified Network.HTTP.Types            as HTTP
import qualified Servant.Auth.Server           as SAuth
import qualified Smart.Server.Page.Navbar      as Nav
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A
import           Text.Blaze.Renderer.Text       ( renderMarkup )
import           Web.FormUrlEncoded             ( FromForm(..)
                                                , parseMaybe
                                                , parseUnique
                                                )
import           Web.HttpApiData                ( FromHttpApiData(..) )


--------------------------------------------------------------------------------
-- | Represents the input data used for user registration. This in effect is
-- the operation to create a user.
data Signup = Signup
  { username   :: UserName
  , password   :: Password
  , email      :: UserEmailAddr
  , tosConsent :: Bool
  }
  deriving (Generic, Eq, Show)

instance FromForm Signup where
  fromForm f =
    Signup
      <$> parseUnique "username"   f
      <*> parseUnique "password"   f
      <*> parseUnique "email-addr" f
      <*> (   (Just "tos-consent-granted" ==)
          .   fmap T.toLower
          <$> parseMaybe "tos-consent" f
          )

-- | Represents the input data to log in a user.
data Login = Login
  { _loginUsername :: UserName
  , _loginPassword :: Password
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance FromForm Login where
  fromForm f = Login <$> parseUnique "username" f <*> parseUnique "password" f

-- | Represents the input data to update a user profile.
newtype Update = Update
  { _editPassword :: Maybe Password
  }
  deriving (Eq, Show, Generic)

instance FromForm Update where
  fromForm f = Update <$> parseMaybe "password" f


--------------------------------------------------------------------------------
type UserProfile = UserProfile' Credentials UserDisplayName UserEmailAddr Bool

data UserProfile' creds userDisplayName userEmailAddr tosConsent = UserProfile
  { _userProfileId                :: UserId
  , _userProfileCreds             :: creds -- ^ Users credentials
  , _userProfileDisplayName       :: userDisplayName -- ^ User's human friendly name
  , _userProfileEmailAddr         :: userEmailAddr -- ^ User's email address
  , _userProfileEmailAddrVerified :: Maybe Text -- ^ TODO Last date it was checked.
  , _userProfileTosConsent        :: tosConsent
  , _userProfileCompletion1       :: UserCompletion1
  , _userProfileCompletion2       :: UserCompletion2
  , _userProfileRights            :: [AccessRight]
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Represents user credentials.
data Credentials = Credentials
  { _userCredsName     :: UserName
  , _userCredsPassword :: Password
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- For Completion-1 level
data UserCompletion1 = UserCompletion1
  { _userProfilePostalAddress      :: Maybe Text -- ^ Non-structured for now.
  , _userProfileTelephoneNbr       :: Maybe Text
  , _userProfileAddrAndTelVerified :: Maybe Text -- TODO Date.
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- For Completion-2 level
data UserCompletion2 = UserCompletion2
  { _userProfileEId         :: Maybe Text -- TODO Not sure what data this is.
  , _userProfileEIdVerified :: Maybe Text -- TODO Date.
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- Enable/disable some accesses.
data AccessRight = CanVerifyEmailAddr
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The username is an identifier (i.e. it is unique).
newtype UserName = UserName { unUserName :: Text }
                 deriving ( Eq
                          , Show
                          , IsString
                          , FromJSON
                          , ToJSON
                          , H.ToMarkup
                          , H.ToValue
                          ) via Text
                 deriving (FromHttpApiData, FromForm) via W.Wrapped "username" Text

newtype UserDisplayName = UserDisplayName Text
                 deriving ( Eq
                          , Show
                          , IsString
                          , FromJSON
                          , ToJSON
                          , H.ToMarkup
                          , H.ToValue
                          ) via Text
                 deriving (FromHttpApiData, FromForm) via W.Wrapped "display-name" Text

newtype UserEmailAddr = UserEmailAddr Text
                 deriving ( Eq
                          , Show
                          , IsString
                          , FromJSON
                          , ToJSON
                          , H.ToMarkup
                          , H.ToValue
                          ) via Text
                 deriving (FromHttpApiData, FromForm) via W.Wrapped "email-addr" Text

newtype Password = Password (Secret.Secret '[ 'Secret.ToJSONExp] Text)
                 deriving (Eq, IsString) via Text
                 deriving ( FromHttpApiData
                          , FromJSON
                          , ToJSON
                          ) via (Secret.Secret '[ 'Secret.ToJSONExp] Text)
                 deriving stock Show
                 deriving FromForm via W.Wrapped "password" Text

-- | Record ID of the form USER-xxx.
newtype UserId = UserId { unUserId :: Text }
               deriving (Eq, Show, SAuth.ToJWT, SAuth.FromJWT)
               deriving ( IsString
                        , FromJSON
                        , ToJSON
                        , H.ToMarkup
                        , H.ToValue
                        ) via Text
               deriving FromForm via W.Wrapped "user-id" Text

instance Nav.IsNavbarContent UserProfile where
  navbarMarkup UserProfile {..} = do
    greeting
    editProfileLink
    H.hr
   where
    greeting =
      H.div . H.text $ T.unwords ["Hi", _userProfileDisplayName ^. coerced]
    editProfileLink = H.a ! A.href "/settings/profile" $ "Edit profile"

instance Storage.DBIdentity UserProfile where
  type DBId UserProfile = UserId
  dbId = _userProfileId

instance Storage.DBStorageOps UserProfile where
  data DBUpdate UserProfile =
    UserCreate UserProfile
    | UserCreateGeneratingUserId Signup
    | UserDelete UserId
    | UserPasswordUpdate UserId Password
    deriving (Show, Eq)
  
  data DBSelect UserProfile =
    -- | Attempt a user-login using the more ambiguous but more friendly
    -- `UserName` and `Password.
    UserLoginWithUserName Credentials
    -- | Select a user with a known `UserId`.
    | SelectUserById UserId
    -- | Select a user with `UserName`.
    | SelectUserByUserName UserName
    deriving (Show, Eq)

-- | Predicates to filter users.
data Predicate = PredicateEmailAddrToVerify | PredicateHas AccessRight
  deriving (Eq, Show)

applyPredicate PredicateEmailAddrToVerify UserProfile {..} =
  isNothing _userProfileEmailAddrVerified

applyPredicate (PredicateHas a) UserProfile {..} = a `elem` _userProfileRights

data UserErr = UserExists
             | UsernameBlocked -- ^ See `usernameBlocklist`.
             | UserNotFound Text
             | IncorrectUsernameOrPassword
             | EmailAddrAlreadyVerified
             | MissingRight AccessRight
             deriving (Eq, Exception, Show)

instance Errs.IsRuntimeErr UserErr where
  errCode = errCode' . \case
    UserExists                  -> "USER_EXISTS"
    UsernameBlocked             -> "USERNAME_BLOCKED"
    UserNotFound{}              -> "USER_NOT_FOUND"
    IncorrectUsernameOrPassword -> "INCORRECT_CREDENTIALS"
    EmailAddrAlreadyVerified    -> "EMAIL_ADDR_ALREADY_VERIFIED"
    MissingRight a              -> "MISSING_RIGHT_" <> "TODO"
    where errCode' = mappend "ERR.USER"

  httpStatus = \case
    UserExists                  -> HTTP.conflict409
    UsernameBlocked             -> HTTP.conflict409 -- TODO Check relevant code.
    UserNotFound{}              -> HTTP.notFound404
    IncorrectUsernameOrPassword -> HTTP.unauthorized401
    EmailAddrAlreadyVerified    -> HTTP.conflict409
    MissingRight _              -> HTTP.unauthorized401

  userMessage = Just . \case
    UserExists -> LT.toStrict . renderMarkup . H.toMarkup $ Pages.ErrorPage
      409
      "User exists"
      "A user with the same username or ID already exists."
    UsernameBlocked ->
      LT.toStrict . renderMarkup . H.toMarkup $ Pages.ErrorPage
        409 -- TODO
        "Username disallowed"
        "Some usernames are not allowed. Please select another."
    UserNotFound msg -> msg
    IncorrectUsernameOrPassword ->
      LT.toStrict . renderMarkup . H.toMarkup $ Pages.ErrorPage
        401
        "Wrong credentials"
        "The supplied username or password is incorrect."
    EmailAddrAlreadyVerified ->
      LT.toStrict . renderMarkup . H.toMarkup $ Pages.ErrorPage
        409
        "Life-cycle error"
        "The user email address is already verified."
    MissingRight a ->
      LT.toStrict
        .  renderMarkup
        .  H.toMarkup
        $  Pages.ErrorPage 401 "Unauthorized action"
        $  "The user has not the required access right "
        <> show a


--------------------------------------------------------------------------------
makeLenses ''UserCompletion2
makeLenses ''UserCompletion1
makeLenses ''Credentials
makeLenses ''UserProfile'


--------------------------------------------------------------------------------
-- | In addition of dynamic rules (e.g. the username should not already be
-- taken), we disallow some names because they might be used later by the
-- system or the company, or cause confusion (because usernames are used as
-- e.g. smartcoop.sh/<username>).
usernameBlocklist :: [UserName]
usernameBlocklist =
  [ "a"
  , "about"
  , "data"
  , "documentation"
  , "echo"
  , "errors"
  , "forms"
  , "login"
  , "messages"
  , "settings"
  , "signup"
  , "smart"
  , "smartcoop" -- In a real syste, I guess this one should be a username that
                -- we ensure is created.
  , "state"
  , "views"
  ]
