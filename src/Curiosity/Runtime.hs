{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Curiosity.Runtime
  ( IOErr(..)
  , Runtime(..)
  , rConf
  , rDb
  , rLoggers
  , AppM(..)
  , boot
  , boot'
  , handleCommand
  , powerdown
  , readDb
  , readDbSafe
  , saveDb
  , saveDbAs
  , runAppMSafe
  , withRuntimeAtomically
  -- * High-level operations
  , selectUserById
  , createUser
  , checkCredentials
  -- * Servant compat
  , appMHandlerNatTrans
  ) where

import qualified Commence.Multilogging         as ML
import qualified Commence.Runtime.Errors       as Errs
import qualified Commence.Runtime.Storage      as S
import           Commence.Types.Secret          ( (=:=) )
import qualified Control.Concurrent.STM        as STM
import           Control.Lens                  as Lens
import "exceptions" Control.Monad.Catch         ( MonadCatch
                                                , MonadMask
                                                , MonadThrow
                                                )
import qualified Curiosity.Command             as Command
import qualified Curiosity.Data                as Data
import qualified Curiosity.Data.User           as User
import qualified Curiosity.Parse               as Command
import qualified Data.Aeson.Text               as Aeson
import qualified Data.ByteString.Lazy          as BS
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import qualified Network.HTTP.Types            as HTTP
import qualified Servant
import           System.Directory               ( doesFileExist )


--------------------------------------------------------------------------------
-- | The runtime, a central product type that should contain all our runtime
-- supporting values.
data Runtime = Runtime
  { _rConf    :: Command.Conf -- ^ The application configuration.
  , _rDb      :: Data.StmDb Runtime -- ^ The Storage.
  , _rLoggers :: ML.AppNameLoggers -- ^ Multiple loggers to log over.
  }

makeLenses ''Runtime

instance Data.RuntimeHasStmDb Runtime where
  stmDbFromRuntime = _rDb


--------------------------------------------------------------------------------
newtype AppM a = AppM { runAppM :: ReaderT Runtime (ExceptT Errs.RuntimeErr IO) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader Runtime
           , MonadError Errs.RuntimeErr
           , MonadThrow
           , MonadCatch
           , MonadMask
           )

-- | Run the `AppM` computation catching all possible exceptions.
runAppMSafe
  :: forall a m
   . MonadIO m
  => Runtime
  -> AppM a
  -> m (Either Errs.RuntimeErr a)
runAppMSafe runtime AppM {..} =
  liftIO
    . fmap (join . first Errs.RuntimeException)
    . try @SomeException
    . runExceptT
    $ runReaderT runAppM runtime


--------------------------------------------------------------------------------
-- | Boot up a runtime.
boot
  :: MonadIO m
  => Command.Conf -- ^ configuration to boot with.
  -> m (Either Errs.RuntimeErr Runtime)
boot _rConf = do
  _rLoggers <- ML.makeDefaultLoggersWithConf $ _rConf ^. Command.confLogging
  eDb       <- instantiateDb _rConf
  pure $ case eDb of
    Left  err  -> Left err
    Right _rDb -> Right Runtime { .. }

-- | Create a runtime from a given state.
boot' :: MonadIO m => Data.HaskDb Runtime -> FilePath -> m Runtime
boot' db logsPath = do
  let loggingConf = Command.mkLoggingConf logsPath
      _rConf      = Command.defaultConf { Command._confLogging = loggingConf }
  _rDb      <- Data.instantiateStmDb db
  _rLoggers <- ML.makeDefaultLoggersWithConf loggingConf
  pure $ Runtime { .. }

-- | Power down the application: attempting to save the DB state in given file, if possible, and reporting errors otherwise.
powerdown :: MonadIO m => Runtime -> m (Maybe Errs.RuntimeErr)
powerdown runtime@Runtime {..} = do
  mDbSaveErr <- saveDb runtime
  -- finally, close loggers.
  ML.flushAndCloseLoggers _rLoggers
  pure mDbSaveErr

saveDb :: MonadIO m => Runtime -> m (Maybe Errs.RuntimeErr)
saveDb runtime =
  maybe (pure Nothing) (saveDbAs runtime) $ _rConf runtime ^. Command.confDbFile

saveDbAs :: MonadIO m => Runtime -> FilePath -> m (Maybe Errs.RuntimeErr)
saveDbAs runtime fpath = do
  haskDb <- Data.readFullStmDbInHask $ _rDb runtime
  let bs = Data.serialiseDb haskDb
  liftIO (try @SomeException (BS.writeFile fpath bs))
    <&> either (Just . Errs.RuntimeException) (const Nothing)

{- | Instantiate the db.

1. The state is either the empty db, or if a _confDbFile file is specified, is
read from the file.

2. Whenever the application exits, the state is written to disk, if a
_confDbFile is specified.
-}
instantiateDb
  :: forall m
   . MonadIO m
  => Command.Conf
  -> m (Either Errs.RuntimeErr (Data.StmDb Runtime))
instantiateDb Command.Conf {..} = readDbSafe _confDbFile

readDb
  :: forall m
   . MonadIO m
  => Maybe FilePath
  -> m (Either Errs.RuntimeErr (Data.StmDb Runtime))
readDb mpath = case mpath of
  Just fpath -> do
    -- We may want to read the file only when the file exists.
    exists <- liftIO $ doesFileExist fpath
    if (exists) then fromFile fpath else useEmpty
  Nothing -> useEmpty
 where
  fromFile fpath = do
    fdata <- liftIO (BS.readFile fpath)
    -- We may want to deserialise the data only when the data is non-empty.
    if BS.null fdata
      then useEmpty
      else
        Data.deserialiseDb fdata & either (pure . Left . Errs.knownErr) useState
  useEmpty = Right <$> Data.instantiateEmptyStmDb
  useState = fmap Right . Data.instantiateStmDb

-- | A safer version of readDb: this fails if the file doesn't exist or is
-- empty. This helps in catching early mistake, e.g. from user specifying the
-- wrong file name on the command-line.
readDbSafe
  :: forall m
   . MonadIO m
  => Maybe FilePath
  -> m (Either Errs.RuntimeErr (Data.StmDb Runtime))
readDbSafe mpath = case mpath of
  Just fpath -> do
    -- We may want to read the file only when the file exists.
    exists <- liftIO $ doesFileExist fpath
    if exists
      then fromFile fpath
      else pure . Left . Errs.knownErr $ FileDoesntExistErr fpath
  Nothing -> useEmpty
 where
  fromFile fpath = do
    fdata <- liftIO (BS.readFile fpath)
    Data.deserialiseDb fdata & either (pure . Left . Errs.knownErr) useState
  useEmpty = Right <$> Data.instantiateEmptyStmDb
  useState = fmap Right . Data.instantiateStmDb

-- | Natural transformation from some `AppM` in any given mode, to a servant
-- Handler.
appMHandlerNatTrans :: forall a . Runtime -> AppM a -> Servant.Handler a
appMHandlerNatTrans rt appM =
  let
      -- We peel off the AppM + ReaderT layers, exposing our ExceptT
      -- RuntimeErr IO a This is very similar to Servant's Handler:
      -- https://hackage.haskell.org/package/servant-server-0.17/docs/Servant-Server-Internal-Handler.html#t:Handler
      unwrapReaderT          = (`runReaderT` rt) . runAppM $ appM
      -- Map our errors to `ServantError`
      runtimeErrToServantErr = withExceptT Errs.asServantError
  in 
      -- Re-wrap as servant `Handler`
      Servant.Handler $ runtimeErrToServantErr unwrapReaderT


--------------------------------------------------------------------------------
-- | Support for logging for the application
instance ML.MonadAppNameLogMulti AppM where
  askLoggers = asks _rLoggers
  localLoggers modLogger =
    local (over rLoggers . over ML.appNameLoggers $ fmap modLogger)


--------------------------------------------------------------------------------
-- | Handle a single command. The @display@ function and the return type
-- provide some flexibility, so this function can be used in both `cty` and
-- `cty repl`.
handleCommand
  :: MonadIO m => Runtime -> (Text -> m ()) -> Command.Command -> m ExitCode
handleCommand runtime@Runtime {..} display command = do
  case command of
    Command.State useHs -> do
      output <-
        runAppMSafe runtime $ ask >>= Data.readFullStmDbInHaskFromRuntime
      case output of
        Right value -> do
          let value' = if useHs
                then show value
                else LT.toStrict (Aeson.encodeToLazyText value)
          display value'
          pure ExitSuccess
        Left err -> display (show err) >> pure (ExitFailure 1)
    Command.CreateUser input -> do
      output <- runAppMSafe runtime . liftIO . STM.atomically $ createUser
        _rDb
        input
      case output of
        Right muid -> do
          case muid of
            Right (User.UserId uid) -> do
              display $ "User created: " <> uid
              pure ExitSuccess
            Left err -> display (show err) >> pure (ExitFailure 1)
        Left err -> display (show err) >> pure (ExitFailure 1)
    Command.SelectUser useHs uid -> do
      output <- runAppMSafe runtime . liftIO . STM.atomically $ selectUserById
        _rDb
        uid
      case output of
        Right mprofile -> do
          case mprofile of
            Just value -> do
              let value' = if useHs
                    then show value
                    else LT.toStrict (Aeson.encodeToLazyText value)
              display value'
              pure ExitSuccess
            Nothing -> display "No such user." >> pure (ExitFailure 1)
        Left err -> display (show err) >> pure (ExitFailure 1)

    Command.UpdateUser update -> do
      output <-
        runAppMSafe runtime . S.liftTxn @AppM @STM $ S.dbUpdate @AppM @STM
          _rDb
          update
      display $ show output
      pure ExitSuccess
    _ -> do
      display $ "Unhandled command " <> show command
      return $ ExitFailure 1

-- | Given an initial state, applies a list of commands, returning the list of
-- new (intermediate and final) states.
interpret
  :: Data.HaskDb Runtime -> [Command.Command] -> IO [Data.HaskDb Runtime]
interpret = loop []
 where
  -- TODO Maybe it would be more efficient to thread a runtime instate of a
  -- state (that needs to be "booted" over and over again).
  loop acc _  []               = pure $ reverse acc
  loop acc st (command : rest) = do
    runtime <- boot' st "/tmp/curiosity-xxx.log"
    handleCommand runtime display command
    st' <- Data.readFullStmDbInHask $ _rDb runtime
    loop (st' : acc) st' rest
  display = putStrLn -- TODO Accumulate in a list, so it can be returned.
                     -- TODO Is is possible to also log to a list ?

instance S.DBTransaction AppM STM where
  liftTxn =
    liftIO
      . fmap (first Errs.RuntimeException)
      . try @SomeException
      . STM.atomically

--------------------------------------------------------------------------------
-- | Definition of all operations for the UserProfiles (selects and updates)
instance S.DBStorage AppM STM User.UserProfile where

  type Db AppM STM User.UserProfile = Data.StmDb Runtime

  type DBError AppM STM User.UserProfile = User.UserErr

  dbUpdate db@Data.Db {..} = \case

    User.UserCreate input -> second pure <$> createUserFull db input

    User.UserCreateGeneratingUserId input ->
      second pure <$> createUser db input

    User.UserDelete id ->
      S.dbSelect @AppM @STM db (User.SelectUserById id) <&> headMay >>= maybe
        (pure . userNotFound $ show id)
        (fmap Right . deleteUser)
     where
      deleteUser _ =
        modifyUserProfiles id (filter $ (/= id) . S.dbId) _dbUserProfiles

    User.UserPasswordUpdate id newPass ->
      S.dbSelect @AppM @STM db (User.SelectUserById id) <&> headMay >>= maybe
        (pure . userNotFound $ show id)
        (fmap Right . updateUser)
     where
      updateUser _ = modifyUserProfiles id replaceOlder _dbUserProfiles
      setPassword =
        set (User.userProfileCreds . User.userCredsPassword) newPass
      replaceOlder users =
        [ if S.dbId u == id then setPassword u else u | u <- users ]

  dbSelect db = \case

    User.UserLoginWithUserName input -> toList <$> checkCredentials db input

    User.SelectUserById        id    -> toList <$> selectUserById db id

    User.SelectUserByUserName username ->
      toList <$> selectUserByUsername db username

modifyUserProfiles id f userProfiles = STM.modifyTVar userProfiles f $> [id]

selectUserById db id = do
  let usersTVar = Data._dbUserProfiles db
  STM.readTVar usersTVar <&> find ((== id) . S.dbId)

selectUserByUsername
  :: Data.StmDb Runtime -> User.UserName -> STM (Maybe User.UserProfile)
selectUserByUsername db username = do
  let usersTVar = Data._dbUserProfiles db
  users' <- STM.readTVar usersTVar
  pure $ find ((== username) . User._userCredsName . User._userProfileCreds)
              users'

createUser
  :: Data.StmDb Runtime -> User.Signup -> STM (Either User.UserErr User.UserId)
createUser db User.Signup {..} = do
  STM.catchSTM (Right <$> transaction) (pure . Left)
 where
  transaction = do
    newId <- generateUserId db
    let newProfile = User.UserProfile newId
                                      (User.Credentials username password)
                                      "TODO"
                                      email
                                      Nothing
                                      tosConsent
                                      Nothing
                                      Nothing
                                      Nothing
                                      Nothing
                                      Nothing
    -- We fail the transaction if createUserFull returns an error,
    -- so that we don't increment _dbNextUserId.
    createUserFull db newProfile >>= either STM.throwSTM pure

createUserFull
  :: Data.StmDb Runtime
  -> User.UserProfile
  -> STM (Either User.UserErr User.UserId)
createUserFull db newProfile = if username `elem` User.usernameBlocklist
  then pure . Left $ User.UsernameBlocked
  else do
    mprofile <- selectUserById db newProfileId
    case mprofile of
      Just profile -> existsErr
      Nothing      -> createNew
 where
  username     = newProfile ^. User.userProfileCreds . User.userCredsName
  newProfileId = S.dbId newProfile
  createNew    = do
    mprofile <- selectUserByUsername db username
    case mprofile of
      Just profile -> existsErr
      Nothing      -> do
        modifyUsers db (newProfile :)
        pure $ Right newProfileId
  existsErr = pure . Left $ User.UserExists

generateUserId :: forall runtime . Data.StmDb runtime -> STM User.UserId
generateUserId db = do
  let nextUserIdTVar = Data._dbNextUserId db
  STM.stateTVar nextUserIdTVar (\i -> (User.UserId $ "USER-" <> show i, succ i))

modifyUsers
  :: Data.StmDb Runtime -> ([User.UserProfile] -> [User.UserProfile]) -> STM ()
modifyUsers db f = do
  let usersTVar = Data._dbUserProfiles db
  STM.modifyTVar usersTVar f

checkCredentials
  :: Data.StmDb Runtime -> User.Credentials -> STM (Maybe User.UserProfile)
checkCredentials db User.Credentials {..} = do
  mprofile <- selectUserByUsername db _userCredsName
  case mprofile of
    Just profile | checkPassword profile _userCredsPassword ->
      pure $ Just profile
    _ -> pure Nothing

-- TODO Use constant-time string comparison.
checkPassword :: User.UserProfile -> User.Password -> Bool
checkPassword profile (User.Password passInput) = storedPass =:= passInput
 where
  User.Password storedPass =
    profile ^. User.userProfileCreds . User.userCredsPassword

userNotFound = Left . User.UserNotFound . mappend "User not found: "

withRuntimeAtomically f a = ask >>= \rt -> liftIO . STM.atomically $ f rt a

--------------------------------------------------------------------------------
newtype IOErr = FileDoesntExistErr FilePath
  deriving Show

instance Errs.IsRuntimeErr IOErr where
  errCode FileDoesntExistErr{} = "ERR.FILE_NOT_FOUND"
  httpStatus FileDoesntExistErr{} = HTTP.notFound404
  userMessage = Just . \case
    FileDoesntExistErr fpath -> T.unwords ["File doesn't exist:", T.pack fpath]
