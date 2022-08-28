{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies   #-}
module Curiosity.Data
  ( Db(..)
  , StmDb
  , HaskDb
  -- * Constraints
  , RuntimeHasStmDb(..)
  -- * Instantiating databases.
  , emptyHask
  , instantiateStmDb
  , instantiateEmptyStmDb
  , resetStmDb
  -- * Reading values from the database.
  , readFullStmDbInHaskFromRuntime
  , readFullStmDbInHask
  , readFullStmDbInHask'
  -- * Serialising and deseralising DB to bytes.
  , serialiseDb
  , deserialiseDb
  ) where

import Curiosity.Data.Counter as C
import qualified Commence.Runtime.Errors       as E
import qualified Control.Concurrent.STM        as STM
import qualified Curiosity.Data.Business       as Business
import qualified Curiosity.Data.Employment     as Employment
import qualified Curiosity.Data.Invoice        as Invoice
import qualified Curiosity.Data.Legal          as Legal
import qualified Curiosity.Data.User           as User
import           Data.Aeson
import qualified Data.Text                     as T
import qualified Network.HTTP.Types.Status     as S

{- | The central database. The product type contains all values and is
parameterised by @datastore@. The @datastore@ can be the layer dealing with
storage. When it is @Identity@, it just means the data is stored as is. It can,
however, also be an `STM.TVar` if the datastore is to be STM based.

Additionally, we want to parameterise over a @runtime@ type parameter. This is
a container type of the database.
-}
data Db (datastore :: Type -> Type) (runtime :: Type) = Db
  { _dbNextBusinessId   :: C.CounterValue datastore Int
  , _dbBusinessEntities :: datastore [Business.Entity]
  , _dbNextLegalId      :: datastore Int
  , _dbLegalEntities    :: datastore [Legal.Entity]
  , _dbNextUserId       :: datastore Int
  , _dbUserProfiles     :: datastore [User.UserProfile]
  , _dbNextInvoiceId    :: datastore Int
  , _dbInvoices         :: datastore [Invoice.Invoice]
  , _dbNextEmploymentId :: datastore Int
  , _dbEmployments      :: datastore [Employment.Contract]
  }

-- | Hask database type: used for starting the system, values reside in @Hask@
-- (thus `Identity`)
type HaskDb runtime = Db Identity runtime

deriving instance Eq (HaskDb runtime)
deriving instance Show (HaskDb runtime)
deriving instance Generic (HaskDb runtime)
deriving anyclass instance ToJSON (HaskDb runtime)
deriving anyclass instance FromJSON (HaskDb runtime)

-- | Stm database type, used for live example applications, values reside in @STM@
type StmDb runtime = Db STM.TVar runtime

-- | Instantiate a seed database that is empty.
emptyHask :: forall runtime . HaskDb runtime
emptyHask = Db
  (pure 1) (pure mempty)
  (pure 1) (pure mempty)
  (pure 1) (pure mempty)
  (pure 1) (pure mempty)
  (pure 1) (pure mempty)

instantiateStmDb
  :: forall runtime m . MonadIO m => HaskDb runtime -> m (StmDb runtime)
instantiateStmDb Db
  { _dbNextBusinessId   = CounterValue (Identity seedNextBusinessId)
  , _dbBusinessEntities = Identity seedBusinessEntities
  , _dbNextLegalId      = Identity seedNextLegalId
  , _dbLegalEntities    = Identity seedLegalEntities
  , _dbNextUserId       = Identity seedNextUserId
  , _dbUserProfiles     = Identity seedProfiles
  , _dbNextInvoiceId    = Identity seedNextInvoiceId
  , _dbInvoices         = Identity seedInvoices
  , _dbNextEmploymentId = Identity seedNextEmploymentId
  , _dbEmployments      = Identity seedEmployments
  }
  =
  -- We don't use `newTVarIO` repeatedly under here and instead wrap the whole
  -- instantiation under a single STM transaction (@atomically@).
    liftIO . STM.atomically $ do
    _dbNextBusinessId   <- C.newCounter seedNextBusinessId
    _dbBusinessEntities <- STM.newTVar seedBusinessEntities
    _dbNextLegalId      <- STM.newTVar seedNextLegalId
    _dbLegalEntities    <- STM.newTVar seedLegalEntities
    _dbNextUserId       <- STM.newTVar seedNextUserId
    _dbUserProfiles     <- STM.newTVar seedProfiles
    _dbNextInvoiceId    <- STM.newTVar seedNextInvoiceId
    _dbInvoices         <- STM.newTVar seedInvoices
    _dbNextEmploymentId <- STM.newTVar seedNextEmploymentId
    _dbEmployments      <- STM.newTVar seedEmployments
    pure Db { .. }

instantiateEmptyStmDb :: forall runtime m . MonadIO m => m (StmDb runtime)
instantiateEmptyStmDb = instantiateStmDb emptyHask

-- | Reset all values of the `Db` product type from `STM.STM` to the empty
-- state.
resetStmDb
  :: forall runtime m . MonadIO m => StmDb runtime -> m ()
resetStmDb stmDb = liftIO . STM.atomically $ do
  STM.writeTVar (_dbNextUserId stmDb) seedNextUserId
  STM.writeTVar (_dbUserProfiles stmDb) seedProfiles
 where
  Db { _dbNextUserId = Identity seedNextUserId, _dbUserProfiles = Identity seedProfiles } = emptyHask

-- | Reads all values of the `Db` product type from `STM.STM` to @Hask@.
readFullStmDbInHaskFromRuntime
  :: forall runtime m
   . (MonadIO m, RuntimeHasStmDb runtime)
  => runtime
  -> m (HaskDb runtime)
readFullStmDbInHaskFromRuntime = readFullStmDbInHask . stmDbFromRuntime
{-# INLINE readFullStmDbInHaskFromRuntime #-}

-- | Reads all values of the `Db` product type from `STM.STM` to @Hask@.
readFullStmDbInHask
  :: forall runtime m . MonadIO m => StmDb runtime -> m (HaskDb runtime)
readFullStmDbInHask = liftIO . STM.atomically . readFullStmDbInHask'

readFullStmDbInHask' stmDb = do
  _dbNextBusinessId   <- pure <$> C.readCounter (_dbNextBusinessId stmDb)
  _dbBusinessEntities <- pure <$> STM.readTVar (_dbBusinessEntities stmDb)
  _dbNextLegalId      <- pure <$> STM.readTVar (_dbNextLegalId stmDb)
  _dbLegalEntities    <- pure <$> STM.readTVar (_dbLegalEntities stmDb)
  _dbNextUserId       <- pure <$> STM.readTVar (_dbNextUserId stmDb)
  _dbUserProfiles     <- pure <$> STM.readTVar (_dbUserProfiles stmDb)
  _dbNextInvoiceId    <- pure <$> STM.readTVar (_dbNextInvoiceId stmDb)
  _dbInvoices         <- pure <$> STM.readTVar (_dbInvoices stmDb)
  _dbNextEmploymentId <- pure <$> STM.readTVar (_dbNextEmploymentId stmDb)
  _dbEmployments      <- pure <$> STM.readTVar (_dbEmployments stmDb)
  pure Db { .. }

{- | Provides us with the ability to constrain on a larger product-type (the
@runtime@) to contain, in some form or another, a value of the `StmDb`, which
can be accessed from the @runtime@.

This solves cyclic imports, without caring about the concrete @runtime@ types,
we can just rely on the constraints.
-}
class RuntimeHasStmDb runtime where
  stmDbFromRuntime :: runtime -> StmDb runtime

newtype DbErr = DbDecodeFailed Text
              deriving Show

instance E.IsRuntimeErr DbErr where
  errCode = errCode' . \case
    DbDecodeFailed{} -> "DECODE_FAILED"
    where errCode' = mappend "ERR.DB."
  httpStatus = \case
    DbDecodeFailed{} -> S.internalServerError500
  userMessage = Just . \case
    DbDecodeFailed msg -> msg

-- | Write an entire db state to bytes.
serialiseDb :: forall runtime . HaskDb runtime -> LByteString
serialiseDb = encode
{-# INLINE serialiseDb #-}

-- | Read an entire db state from bytes.
deserialiseDb :: forall runtime . LByteString -> Either DbErr (HaskDb runtime)
deserialiseDb = first (DbDecodeFailed . T.pack) . eitherDecode
{-# INLINE deserialiseDb #-}
