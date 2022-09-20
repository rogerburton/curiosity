{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Curiosity.Runtime
  ( IOErr(..)
  , UnspeciedErr(..)
  , Runtime(..)
  , rConf
  , rDb
  , rLoggers
  , AppM(..)
  , boot
  , boot'
  , reset
  , handleCommand
  , powerdown
  , readDb
  , readDbSafe
  , saveDb
  , saveDbAs
  , runAppMSafe
  , withRuntimeAtomically
  -- * High-level user operations
  , canPerform
  , setUserEmailAddrAsVerifiedFull
  , selectUserById
  , selectUserByUsername
  , filterUsers
  , createUser
  , checkCredentials
  -- * High-level entity operations
  , selectEntityBySlug
  -- * High-level unit operations
  , selectUnitBySlug
  -- * Form edition
  -- ** Contract
  , newCreateContractForm
  , readCreateContractForm
  , writeCreateContractForm
  , addExpenseToContractForm
  , writeExpenseToContractForm
  , removeExpenseFromContractForm
  -- ** Simple Contract
  , newCreateSimpleContractForm
  , readCreateSimpleContractForm
  , addRoleToSimpleContractForm
  -- * ID generation
  , generateUserId
  , firstUserId
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
import qualified Curiosity.Data.Business       as Business
import qualified Curiosity.Data.Counter        as C
import qualified Curiosity.Data.Employment     as Employment
import qualified Curiosity.Data.Invoice        as Invoice
import qualified Curiosity.Data.Legal          as Legal
import qualified Curiosity.Data.SimpleContract as SimpleContract
import qualified Curiosity.Data.User           as User
import qualified Curiosity.Parse               as Command
import qualified Data.Aeson.Text               as Aeson
import qualified Data.ByteString.Lazy          as BS
import qualified Data.Map                      as M
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import qualified Data.Text.IO                  as T
import qualified Data.Text.Lazy                as LT
import qualified Language.Haskell.TH.Syntax    as Syntax
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
boot _rConf =
  liftIO
      (  try @SomeException
      .  ML.makeDefaultLoggersWithConf
      $  _rConf
      ^. Command.confLogging
      )
    >>= \case
          Left loggerErrs ->
            putStrLn @Text
                (  "Cannot instantiate, logger instantiation failed: "
                <> show loggerErrs
                )
              $> Left (Errs.RuntimeException loggerErrs)
          Right _rLoggers -> do
            eDb <- instantiateDb _rConf
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

-- | Reset the database to the empty state
reset runtime = do
  Data.resetStmDb $ _rDb runtime

-- | Power down the application: attempting to save the DB state in given file, if possible, and reporting errors otherwise.
powerdown :: MonadIO m => Runtime -> m (Maybe Errs.RuntimeErr)
powerdown runtime@Runtime {..} = do
  mDbSaveErr <- saveDb runtime
  reportDbSaveErr mDbSaveErr
  -- finally, close loggers.
  eLoggingCloseErrs <- liftIO . try @SomeException $ ML.flushAndCloseLoggers
    _rLoggers
  reportLoggingCloseErrs eLoggingCloseErrs
  pure $ mDbSaveErr <|> first Errs.RuntimeException eLoggingCloseErrs ^? _Left
 where
  reportLoggingCloseErrs eLoggingCloseErrs =
    when (isLeft eLoggingCloseErrs)
      .  putStrLn @Text
      $  "Errors when closing loggers: "
      <> show eLoggingCloseErrs
  reportDbSaveErr mDbSaveErr =
    when (isJust mDbSaveErr)
      .  putStrLn @Text
      $  "DB state cannot be saved: "
      <> show mDbSaveErr

saveDb :: MonadIO m => Runtime -> m (Maybe Errs.RuntimeErr)
saveDb runtime =
  maybe (pure Nothing) (saveDbAs runtime) $ _rConf runtime ^. Command.confDbFile

saveDbAs :: MonadIO m => Runtime -> FilePath -> m (Maybe Errs.RuntimeErr)
saveDbAs runtime fpath = do
  haskDb <- Data.readFullStmDbInHask $ _rDb runtime
  let bs = Data.serialiseDb haskDb
  liftIO
      (try @SomeException (T.writeFile fpath . TE.decodeUtf8 $ BS.toStrict bs))
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
    if exists then fromFile fpath else useEmpty
  Nothing -> useEmpty
 where
  fromFile fpath = liftIO (try @SomeException $ T.readFile fpath) >>= \case
    Left err ->
      putStrLn @Text ("Unable to read db file: " <> maybe "" T.pack mpath)
        $> Left (Errs.RuntimeException err)
    Right fdata ->
           -- We may want to deserialise the data only when the data is non-empty.
                   if T.null fdata
      then useEmpty
      else
        Data.deserialiseDbStrict (TE.encodeUtf8 fdata)
          & either (pure . Left . Errs.knownErr) useState
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
    fdata <- liftIO (T.readFile fpath)
    Data.deserialiseDbStrict (TE.encodeUtf8 fdata)
      & either (pure . Left . Errs.knownErr) useState
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
-- | Handle a single command. The return type provides some flexibility, so
-- this function can be used in both `cty` and `cty repl`.
handleCommand
  :: MonadIO m
  => Runtime
  -> User.UserName -- ^ The user performing the command.
  -> Command.Command
  -> m (ExitCode, [Text])
handleCommand runtime@Runtime {..} user command = do
  case command of
    Command.State useHs -> do
      output <-
        runAppMSafe runtime $ ask >>= Data.readFullStmDbInHaskFromRuntime
      case output of
        Right value -> do
          let value' = if useHs
                then show value
                else LT.toStrict (Aeson.encodeToLazyText value)
          pure (ExitSuccess, [value'])
        Left err -> pure (ExitFailure 1, [show err])
    Command.CreateBusinessEntity input -> do
      output <- runAppMSafe runtime . liftIO . STM.atomically $ createBusiness
        _rDb
        input
      case output of
        Right mid -> do
          case mid of
            Right (Business.BusinessId id) -> do
              pure (ExitSuccess, ["Business entity created: " <> id])
            Left err -> pure (ExitFailure 1, [show err])
        Left err -> pure (ExitFailure 1, [show err])
    Command.UpdateBusinessEntity input -> do
      output <- runAppMSafe runtime . liftIO . STM.atomically $ updateBusiness
        _rDb
        input
      case output of
        Right mid -> do
          case mid of
            Right () -> do
              pure
                ( ExitSuccess
                , ["Business unit updated: " <> Business._updateSlug input]
                )
            Left err -> pure (ExitFailure 1, [show err])
        Left err -> pure (ExitFailure 1, [show err])
    Command.CreateLegalEntity input -> do
      output <- runAppMSafe runtime . liftIO . STM.atomically $ createLegal
        _rDb
        input
      case output of
        Right mid -> do
          case mid of
            Right (Legal.LegalId id) -> do
              pure (ExitSuccess, ["Legal entity created: " <> id])
            Left err -> pure (ExitFailure 1, [show err])
        Left err -> pure (ExitFailure 1, [show err])
    Command.UpdateLegalEntity input -> do
      output <- runAppMSafe runtime . liftIO . STM.atomically $ updateLegal
        _rDb
        input
      case output of
        Right mid -> do
          case mid of
            Right () -> do
              pure
                ( ExitSuccess
                , ["Legal entity updated: " <> Legal._updateSlug input]
                )
            Left err -> pure (ExitFailure 1, [show err])
        Left err -> pure (ExitFailure 1, [show err])
    Command.CreateUser input -> do
      output <- runAppMSafe runtime . liftIO . STM.atomically $ createUser
        _rDb
        input
      case output of
        Right muid -> do
          case muid of
            Right (User.UserId uid) -> do
              pure (ExitSuccess, ["User created: " <> uid])
            Left err -> pure (ExitFailure 1, [show err])
        Left err -> pure (ExitFailure 1, [show err])
    Command.SelectUser useHs uid short -> do
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
              if short
                then pure
                  ( ExitSuccess
                  , [ User.unUserId (User._userProfileId value)
                      <> " "
                      <> User.unUserName
                           (User._userCredsName $ User._userProfileCreds value)
                    ]
                  )
                else pure (ExitSuccess, [value'])
            Nothing -> pure (ExitFailure 1, ["No such user."])
        Left err -> pure (ExitFailure 1, [show err])
    Command.FilterUsers predicate -> do
      output <- runAppMSafe runtime
        $ withRuntimeAtomically filterUsers predicate
      case output of
        Right profiles -> do
          let f User.UserProfile {..} =
                let User.UserId   i = _userProfileId
                    User.UserName n = User._userCredsName _userProfileCreds
                in  i <> " " <> n
          pure (ExitSuccess, map f profiles)
    Command.UpdateUser input -> do
      output <-
        runAppMSafe runtime . S.liftTxn @AppM @STM $ S.dbUpdate @AppM @STM
          _rDb
          input
      pure (ExitSuccess, [show output])
    Command.SetUserEmailAddrAsVerified username -> do
      output <-
        runAppMSafe runtime
        .   liftIO
        .   STM.atomically
        $   selectUserByUsername _rDb user
        >>= \case
              Just profile ->
                setUserEmailAddrAsVerifiedFull _rDb (profile, username)
              Nothing -> pure . Left . User.UserNotFound $ User.unUserName user
      case output of
        Right (Right ()) -> do
          pure (ExitSuccess, ["User successfully updated."])
        Right (Left err) -> pure (ExitFailure 1, [show err])
        Left  err        -> pure (ExitFailure 1, [show err])
    Command.CreateEmployment input -> do
      output <-
        runAppMSafe runtime
        .   liftIO
        .   STM.atomically
        $   selectUserByUsername _rDb user
        >>= \case
              Just profile -> do
                merror <- submitCreateContractForm' _rDb (profile, input)
                -- TODO Should we have a type to combine multiple possible errors ?
                case merror of
                  Left (Employment.Err err) ->
                    pure . Left $ User.UserNotFound err
                  Right id -> pure $ Right id
              Nothing -> pure . Left . User.UserNotFound $ User.unUserName user
      case output of
        Right mid -> do
          case mid of
            Right (Employment.ContractId id) -> do
              pure (ExitSuccess, ["Employment contract created: " <> id])
            Left err -> pure (ExitFailure 1, [show err])
        Left err -> pure (ExitFailure 1, [show err])
    Command.CreateInvoice -> do
      output <- runAppMSafe runtime . liftIO . STM.atomically $ createInvoice
        _rDb
      case output of
        Right mid -> do
          case mid of
            Right (Invoice.InvoiceId id) -> do
              pure (ExitSuccess, ["Invoice created: " <> id])
            Left err -> pure (ExitFailure 1, [show err])
        Left err -> pure (ExitFailure 1, [show err])
    Command.Step -> do
      let transaction rt _ = do
            users <- filterUsers rt User.PredicateEmailAddrToVerify
            mapM
              ( setUserEmailAddrAsVerified _rDb
              . User._userCredsName
              . User._userProfileCreds
              )
              users
      output <- runAppMSafe runtime $ withRuntimeAtomically transaction ()
      case output of
        Right x   -> pure (ExitSuccess, map show x)
        Left  err -> pure (ExitFailure 1, [Errs.displayErr err])
    _ -> do
      -- TODO It seems that showing the command causes a stack overflow. For
      -- instance the error happens by passing the Log or the Reset commands.
      -- I think this has to do with the fact that they contain a conf.
      -- pure (ExitFailure 1, ["Unhandled command " <> show command])
      pure (ExitFailure 1, ["Unhandled command."])

setUserEmailAddrAsVerifiedFull
  :: Data.StmDb runtime
  -> (User.UserProfile, User.UserName)
  -> STM (Either User.UserErr ())
setUserEmailAddrAsVerifiedFull db (user, input) = transaction
 where
  transaction = do
    b <- canPerform 'User.SetUserEmailAddrAsVerified db user
    if b
      then setUserEmailAddrAsVerified db input
      else pure . Left $ User.MissingRight User.CanVerifyEmailAddr

canPerform :: Syntax.Name -> Data.StmDb runtime -> User.UserProfile -> STM Bool
canPerform action db User.UserProfile {..}
  | action == 'User.SetUserEmailAddrAsVerified
  = pure $ User.CanVerifyEmailAddr `elem` _userProfileRights
  | otherwise
  = pure False

-- | Given an initial state, applies a list of commands, returning the list of
-- new (intermediate and final) states.
interpret
  :: Data.HaskDb Runtime
  -> User.UserName
  -> [Command.Command]
  -> IO [(ExitCode, [Text], Data.HaskDb Runtime)]
interpret = loop []
 where
  -- TODO Maybe it would be more efficient to thread a runtime instate of a
  -- state (that needs to be "booted" over and over again).
  loop acc _  _    []               = pure $ reverse acc
  loop acc st user (command : rest) = do
    runtime        <- boot' st "/tmp/curiosity-xxx.log"
    (code, output) <- handleCommand runtime user command
    st'            <- Data.readFullStmDbInHask $ _rDb runtime
    loop ((code, output, st') : acc) st' user rest
                     -- TODO Is is possible to also log to a list ?

instance S.DBTransaction AppM STM where
  liftTxn =
    liftIO
      . fmap (first Errs.RuntimeException)
      . try @SomeException
      . STM.atomically


--------------------------------------------------------------------------------
createBusiness
  :: forall runtime
   . Data.StmDb runtime
  -> Business.Create
  -> STM (Either Business.Err Business.BusinessId)
createBusiness db Business.Create {..} = do
  STM.catchSTM (Right <$> transaction) (pure . Left)
 where
  transaction = do
    newId <- generateBusinessId db
    let new = Business.Entity newId _createSlug _createName Nothing
    createBusinessFull db new >>= either STM.throwSTM pure

createBusinessFull
  :: forall runtime
   . Data.StmDb runtime
  -> Business.Entity
  -> STM (Either Business.Err Business.BusinessId)
createBusinessFull db new = do
  modifyBusinessEntities db (++ [new])
  pure . Right $ Business._entityId new

updateBusiness db Business.Update {..} = do
  mentity <- selectUnitBySlug db _updateSlug
  case mentity of
    Just Business.Entity {..} -> do
      let replaceOlder entities =
            [ if Business._entitySlug e == _updateSlug
                then e { Business._entityDescription = _updateDescription }
                else e
            | e <- entities
            ]
      modifyBusinessEntities db replaceOlder
      pure $ Right ()
    Nothing -> pure . Left $ User.UserNotFound _updateSlug -- TODO

generateBusinessId
  :: forall runtime . Data.StmDb runtime -> STM Business.BusinessId
generateBusinessId Data.Db {..} =
  Business.BusinessId <$> C.bumpCounterPrefix "BENT-" _dbNextBusinessId

modifyBusinessEntities
  :: forall runtime
   . Data.StmDb runtime
  -> ([Business.Entity] -> [Business.Entity])
  -> STM ()
modifyBusinessEntities db f =
  let tvar = Data._dbBusinessEntities db in STM.modifyTVar tvar f


--------------------------------------------------------------------------------
createLegal
  :: forall runtime
   . Data.StmDb runtime
  -> Legal.Create
  -> STM (Either Legal.Err Legal.LegalId)
createLegal db Legal.Create {..} = do
  STM.catchSTM (Right <$> transaction) (pure . Left)
 where
  transaction = do
    newId <- generateLegalId db
    let new = Legal.Entity newId
                           _createSlug
                           _createName
                           _createCbeNumber
                           _createVatNumber
                           Nothing
    createLegalFull db new >>= either STM.throwSTM pure

createLegalFull
  :: forall runtime
   . Data.StmDb runtime
  -> Legal.Entity
  -> STM (Either Legal.Err Legal.LegalId)
createLegalFull db new = do
  modifyLegalEntities db (++ [new])
  pure . Right $ Legal._entityId new

updateLegal db Legal.Update {..} = do
  mentity <- selectEntityBySlug db _updateSlug
  case mentity of
    Just Legal.Entity {..} -> do
      let replaceOlder entities =
            [ if Legal._entitySlug e == _updateSlug
                then e { Legal._entityDescription = _updateDescription }
                else e
            | e <- entities
            ]
      modifyLegalEntities db replaceOlder
      pure $ Right ()
    Nothing -> pure . Left $ User.UserNotFound _updateSlug -- TODO

generateLegalId :: forall runtime . Data.StmDb runtime -> STM Legal.LegalId
generateLegalId Data.Db {..} =
  Legal.LegalId <$> C.bumpCounterPrefix "LENT-" _dbNextLegalId

modifyLegalEntities
  :: forall runtime
   . Data.StmDb runtime
  -> ([Legal.Entity] -> [Legal.Entity])
  -> STM ()
modifyLegalEntities db f =
  let tvar = Data._dbLegalEntities db in STM.modifyTVar tvar f


--------------------------------------------------------------------------------
createEmployment
  :: forall runtime
   . Data.StmDb runtime
  -> Employment.Contract
  -> STM (Either Employment.Err Employment.ContractId)
createEmployment db c = do
  STM.catchSTM (Right <$> transaction) (pure . Left)
 where
  transaction = do
    newId <- generateEmploymentId db
    let new = Employment.Contract newId
    createEmploymentFull db new >>= either STM.throwSTM pure

createEmploymentFull
  :: forall runtime
   . Data.StmDb runtime
  -> Employment.Contract
  -> STM (Either Employment.Err Employment.ContractId)
createEmploymentFull db new = do
  modifyEmployments db (++ [new])
  pure . Right $ Employment._contractId new

generateEmploymentId
  :: forall runtime . Data.StmDb runtime -> STM Employment.ContractId
generateEmploymentId Data.Db {..} =
  Employment.ContractId <$> C.bumpCounterPrefix "EMP-" _dbNextEmploymentId

modifyEmployments
  :: forall runtime
   . Data.StmDb runtime
  -> ([Employment.Contract] -> [Employment.Contract])
  -> STM ()
modifyEmployments db f =
  let tvar = Data._dbEmployments db in STM.modifyTVar tvar f

newCreateContractForm
  :: forall runtime
   . Data.StmDb runtime
  -> (User.UserProfile, Employment.CreateContractAll')
  -> STM Text
newCreateContractForm db (profile, Employment.CreateContractAll' gi ty ld rs inv)
  = do
    key <- Data.genRandomText db
    STM.modifyTVar (Data._dbFormCreateContractAll db) (add key)
    pure key
 where
  add key =
    M.insert (username, key) (Employment.CreateContractAll gi ty ld rs inv [])
  username = User._userCredsName $ User._userProfileCreds profile

readCreateContractForm
  :: forall runtime
   . Data.StmDb runtime
  -> (User.UserProfile, Text)
  -> STM (Either () Employment.CreateContractAll)
readCreateContractForm db (profile, key) = do
  m <- STM.readTVar $ Data._dbFormCreateContractAll db
  let mform = M.lookup (username, key) m
  pure $ maybe (Left ()) Right mform
  where username = User._userCredsName $ User._userProfileCreds profile

writeCreateContractForm
  :: forall runtime
   . Data.StmDb runtime
  -> (User.UserProfile, Text, Employment.CreateContractAll')
  -> STM Text
writeCreateContractForm db (profile, key, Employment.CreateContractAll' gi ty ld rs inv)
  = do
    STM.modifyTVar (Data._dbFormCreateContractAll db) save
    pure key
 where
  -- TODO Return an error when the key is not found.
  save = M.adjust
    (\(Employment.CreateContractAll _ _ _ _ _ es) ->
      Employment.CreateContractAll gi ty ld rs inv es
    )
    (username, key)
  username = User._userCredsName $ User._userProfileCreds profile

addExpenseToContractForm
  :: forall runtime
   . Data.StmDb runtime
  -> (User.UserProfile, Text, Employment.AddExpense)
  -> STM () -- TODO Possible errors
addExpenseToContractForm db (profile, key, expense) = do
  STM.modifyTVar (Data._dbFormCreateContractAll db) save
 where
  save = M.adjust
    (\(Employment.CreateContractAll gi ty ld rs inv es) ->
      Employment.CreateContractAll gi ty ld rs inv $ es ++ [expense]
    )
    (username, key)
  username = User._userCredsName $ User._userProfileCreds profile

writeExpenseToContractForm
  :: forall runtime
   . Data.StmDb runtime
  -> (User.UserProfile, Text, Int, Employment.AddExpense)
  -> STM () -- TODO Possible errors
writeExpenseToContractForm db (profile, key, index, expense) = do
  STM.modifyTVar (Data._dbFormCreateContractAll db) save
 where
  save = M.adjust
    (\(Employment.CreateContractAll gi ty ld rs inv es) ->
      let f i e = if i == index then expense else e
          es' = zipWith f [0 ..] es
      in  Employment.CreateContractAll gi ty ld rs inv es'
    )
    (username, key)
  username = User._userCredsName $ User._userProfileCreds profile

removeExpenseFromContractForm
  :: forall runtime
   . Data.StmDb runtime
  -> (User.UserProfile, Text, Int)
  -> STM () -- TODO Possible errors
removeExpenseFromContractForm db (profile, key, index) = do
  STM.modifyTVar (Data._dbFormCreateContractAll db) save
 where
  save = M.adjust
    (\(Employment.CreateContractAll gi ty ld rs inv es) ->
      let es' = map snd . filter ((/= index) . fst) $ zip [0 ..] es
      in  Employment.CreateContractAll gi ty ld rs inv es'
    )
    (username, key)
  username = User._userCredsName $ User._userProfileCreds profile

-- | Fetch the contract form from the staging area, then attempt to validate
-- and create it.
submitCreateContractForm
  :: forall runtime
   . Data.StmDb runtime
  -> (User.UserProfile, Employment.SubmitContract)
  -> STM (Either Employment.Err Employment.ContractId)
submitCreateContractForm db (profile, Employment.SubmitContract key) = do
  minput <- readCreateContractForm db (profile, key)
  case minput of
    Right input -> submitCreateContractForm' db (profile, input)
    Left  err   -> pure . Left $ Employment.Err (show err)

-- | Attempt to create a contract form and create it.
submitCreateContractForm'
  :: forall runtime
   . Data.StmDb runtime
  -> (User.UserProfile, Employment.CreateContractAll)
  -> STM (Either Employment.Err Employment.ContractId)
submitCreateContractForm' db (profile, input) = do
  let mc = Employment.validateCreateContract profile input
  case mc of
    Right c   -> createEmployment db c
    Left  err -> pure . Left $ Employment.Err (show err)


--------------------------------------------------------------------------------
newCreateSimpleContractForm
  :: forall runtime
   . Data.StmDb runtime
  -> (User.UserProfile, SimpleContract.CreateContractAll')
  -> STM Text
newCreateSimpleContractForm db (profile, SimpleContract.CreateContractAll' ty ld rs inv)
  = do
    key <- Data.genRandomText db
    STM.modifyTVar (Data._dbFormCreateSimpleContractAll db) (add key)
    pure key
 where
  add key =
    M.insert (username, key) (SimpleContract.CreateContractAll ty ld rs inv [])
  username = User._userCredsName $ User._userProfileCreds profile

readCreateSimpleContractForm
  :: forall runtime
   . Data.StmDb runtime
  -> (User.UserProfile, Text)
  -> STM (Either () SimpleContract.CreateContractAll)
readCreateSimpleContractForm db (profile, key) = do
  m <- STM.readTVar $ Data._dbFormCreateSimpleContractAll db
  let mform = M.lookup (username, key) m
  pure $ maybe (Left ()) Right mform
  where username = User._userCredsName $ User._userProfileCreds profile

addRoleToSimpleContractForm
  :: forall runtime
   . Data.StmDb runtime
  -> (User.UserProfile, Text, SimpleContract.SelectRole)
  -> STM () -- TODO Possible errors
addRoleToSimpleContractForm db (profile, key, SimpleContract.SelectRole role) =
  do
    STM.modifyTVar (Data._dbFormCreateSimpleContractAll db) save
 where
  save = M.adjust
    (\(SimpleContract.CreateContractAll ty ld rs inv es) ->
      let ty' = ty { SimpleContract._createContractRole = role }
      in  SimpleContract.CreateContractAll ty' ld rs inv es
    )
    (username, key)
  username = User._userCredsName $ User._userProfileCreds profile


--------------------------------------------------------------------------------
createInvoice
  :: forall runtime
   . Data.StmDb runtime
  -> STM (Either Invoice.Err Invoice.InvoiceId)
createInvoice db = do
  STM.catchSTM (Right <$> transaction) (pure . Left)
 where
  transaction = do
    newId <- generateInvoiceId db
    let new = Invoice.Invoice newId
    createInvoiceFull db new >>= either STM.throwSTM pure

createInvoiceFull
  :: forall runtime
   . Data.StmDb runtime
  -> Invoice.Invoice
  -> STM (Either Invoice.Err Invoice.InvoiceId)
createInvoiceFull db new = do
  modifyInvoices db (++ [new])
  pure . Right $ Invoice._entityId new

generateInvoiceId
  :: forall runtime . Data.StmDb runtime -> STM Invoice.InvoiceId
generateInvoiceId Data.Db {..} =
  Invoice.InvoiceId <$> C.bumpCounterPrefix "INV-" _dbNextInvoiceId

modifyInvoices
  :: forall runtime
   . Data.StmDb runtime
  -> ([Invoice.Invoice] -> [Invoice.Invoice])
  -> STM ()
modifyInvoices db f = let tvar = Data._dbInvoices db in STM.modifyTVar tvar f


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

    User.UserUpdate id (User.Update mname mbio) ->
      S.dbSelect @AppM @STM db (User.SelectUserById id) <&> headMay >>= maybe
        (pure . userNotFound $ show id)
        (fmap Right . updateUser)
     where
      updateUser _ = modifyUserProfiles id replaceOlder _dbUserProfiles
      change =
        set User.userProfileBio mbio . set User.userProfileDisplayName mname
      replaceOlder users =
        [ if S.dbId u == id then change u else u | u <- users ]

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
  :: forall runtime
   . Data.StmDb runtime
  -> User.UserName
  -> STM (Maybe User.UserProfile)
selectUserByUsername db username = do
  let usersTVar = Data._dbUserProfiles db
  users' <- STM.readTVar usersTVar
  pure $ find ((== username) . User._userCredsName . User._userProfileCreds)
              users'

filterUsers :: Runtime -> User.Predicate -> STM [User.UserProfile]
filterUsers runtime predicate = do
  let usersTVar = Data._dbUserProfiles $ _rDb runtime
  users' <- STM.readTVar usersTVar
  pure $ filter (User.applyPredicate predicate) users'

createUser
  :: forall runtime
   . Data.StmDb runtime
  -> User.Signup
  -> STM (Either User.UserErr User.UserId)
createUser db User.Signup {..} = do
  STM.catchSTM (Right <$> transaction) (pure . Left)
 where
  transaction = do
    newId <- generateUserId db
    let newProfile = User.UserProfile
          newId
          (User.Credentials username password)
          Nothing
          Nothing
          email
          Nothing
          tosConsent
          (User.UserCompletion1 Nothing Nothing Nothing)
          (User.UserCompletion2 Nothing Nothing)
          -- The very first user has plenty of rights:
          (if newId == firstUserId then firstUserRights else [])
    -- We fail the transaction if createUserFull returns an error,
    -- so that we don't increment _dbNextUserId.
    createUserFull db newProfile >>= either STM.throwSTM pure

createUserFull
  :: forall runtime
   . Data.StmDb runtime
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
        modifyUsers db (++ [newProfile])
        pure $ Right newProfileId
  existsErr = pure . Left $ User.UserExists

generateUserId :: forall runtime . Data.StmDb runtime -> STM User.UserId
generateUserId Data.Db {..} =
  User.UserId <$> C.bumpCounterPrefix "USER-" _dbNextUserId

firstUserId :: User.UserId
firstUserId = "USER-1"

firstUserRights :: [User.AccessRight]
firstUserRights = [User.CanCreateContracts, User.CanVerifyEmailAddr]

modifyUsers
  :: forall runtime
   . Data.StmDb runtime
  -> ([User.UserProfile] -> [User.UserProfile])
  -> STM ()
modifyUsers db f =
  let usersTVar = Data._dbUserProfiles db in STM.modifyTVar usersTVar f

setUserEmailAddrAsVerified
  :: forall runtime
   . Data.StmDb runtime
  -> User.UserName
  -> STM (Either User.UserErr ())
setUserEmailAddrAsVerified db username = do
  mprofile <- selectUserByUsername db username
  case mprofile of
    Just User.UserProfile {..} -> case _userProfileEmailAddrVerified of
      Nothing -> do
        let replaceOlder users =
              [ if User._userCredsName (User._userProfileCreds u) == username
                  then u { User._userProfileEmailAddrVerified = Just "TODO" }
                  else u
              | u <- users
              ]
        modifyUsers db replaceOlder
        pure $ Right ()
      Just _ -> pure . Left $ User.EmailAddrAlreadyVerified
    Nothing -> pure . Left $ User.UserNotFound $ User.unUserName username

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

selectEntityBySlug
  :: forall runtime . Data.StmDb runtime -> Text -> STM (Maybe Legal.Entity)
selectEntityBySlug db name = do
  let tvar = Data._dbLegalEntities db
  records <- STM.readTVar tvar
  pure $ find ((== name) . Legal._entitySlug) records

selectUnitBySlug
  :: forall runtime . Data.StmDb runtime -> Text -> STM (Maybe Business.Entity)
selectUnitBySlug db name = do
  let tvar = Data._dbBusinessEntities db
  records <- STM.readTVar tvar
  pure $ find ((== name) . Business._entitySlug) records

withRuntimeAtomically f a = ask >>= \rt -> liftIO . STM.atomically $ f rt a

--------------------------------------------------------------------------------
newtype IOErr = FileDoesntExistErr FilePath
  deriving Show

instance Errs.IsRuntimeErr IOErr where
  errCode FileDoesntExistErr{} = "ERR.FILE_NOT_FOUND"
  httpStatus FileDoesntExistErr{} = HTTP.notFound404
  userMessage = Just . \case
    FileDoesntExistErr fpath -> T.unwords ["File doesn't exist:", T.pack fpath]

-- | A placeholder error type, used until better handling (at the call site) is
-- put in place.
newtype UnspeciedErr = UnspeciedErr Text
  deriving Show

instance Errs.IsRuntimeErr UnspeciedErr where
  errCode UnspeciedErr{} = "ERR.FILE_NOT_FOUND"
  httpStatus UnspeciedErr{} = HTTP.notFound404
  userMessage = Just . \case
    UnspeciedErr msg -> T.unwords ["Error:", msg]
