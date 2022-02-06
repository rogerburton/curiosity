{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Prototype.Example.Runtime
  ( Conf(..)
  , Runtime(..)
  , ExampleAppM(..)
  ) where

import qualified Control.Concurrent.STM        as STM
import qualified Prototype.Backend.InteractiveState.Repl
                                               as Repl
import qualified Prototype.Example.Data        as Data
import qualified Prototype.Example.Data.Todo   as Todo
import qualified Prototype.Example.Data.User   as User
import qualified Prototype.Runtime.Errors      as Errs
import qualified Prototype.Runtime.Storage     as S
import           Prototype.Types.Secret         ( (=:=) )

newtype Conf = Conf { _confRepl :: Repl.ReplConf }
             deriving Show

-- | The runtime, a central product type that should contain all our runtime supporting values. 
data Runtime = Runtime
  { _rConf :: Conf -- ^ The application configuration.
  , _rDb   :: Data.StmDb Runtime -- ^ The Storage. 
  }

instance Data.RuntimeHasStmDb Runtime where
  stmDbFromRuntime = _rDb

newtype ExampleAppM a = ExampleAppM { runExampleAppM :: ReaderT Runtime (ExceptT Errs.RuntimeErr IO) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader Runtime
           , MonadError Errs.RuntimeErr
           )

-- | Definition of all operations for the UserProfiles (selects and updates)
instance S.DBStorage ExampleAppM User.UserProfile where
  dbUpdate = \case

    User.UserCreate newProfile -> onUserExists newProfileId createNew existsErr
     where
      newProfileId = S.dbId newProfile
      createNew =
        withUserStorage $ modifyUserProfiles newProfileId (newProfile :)
      existsErr = Errs.throwError' . User.UserExists . show

    User.UserDelete id -> onUserExists id (notFound id) deleteUser
     where
      deleteUser _ =
        withUserStorage $ modifyUserProfiles id (filter $ (/= id) . S.dbId)

    User.UserUpdate updatedProfile -> onUserExists id (notFound id) updateUser
     where
      id = S.dbId updatedProfile
      updateUser _ = withUserStorage $ modifyUserProfiles id replaceOlder
      replaceOlder users =
        [ if userId == id then updatedProfile else u
        | u <- users
        , let userId = S.dbId u
        ]

   where
    modifyUserProfiles id f userProfiles =
      liftIO $ STM.atomically (STM.modifyTVar userProfiles f) $> [id]

  dbSelect = \case
    User.UserLogin id (User.UserPassword passInput) -> onUserExists
      id
      (notFound id)
      comparePass
     where
      comparePass foundUser@User.UserProfile { _userProfilePassword = User.UserPassword passStored }
        | passStored =:= passInput
        = pure [foundUser]
        | otherwise
        = Errs.throwError' . User.IncorrectPassword $ "Passwords don't match!"

    User.SelectUserById id ->
      withUserStorage $ liftIO . STM.readTVarIO >=> pure . filter
        ((== id) . S.dbId)

onUserExists id onNone onExisting =
  S.dbSelect (User.SelectUserById id) <&> headMay >>= maybe onNone onExisting
notFound = Errs.throwError' . User.UserNotFound . show
withUserStorage f = asks (Data._dbUserProfiles . _rDb) >>= f
