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
  -- * Pseudo-random number generation.
  , genRandomText
  , readStdGen
  , writeStdGen
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
import qualified Data.List                     as L
import           Data.Map                       ( Map )
import qualified Data.Text                     as T
import qualified Network.HTTP.Types.Status     as S
import qualified System.Random                 as Rand
import qualified System.Random.Internal        as Rand
import qualified System.Random.SplitMix        as SM


--------------------------------------------------------------------------------
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
  , _dbNextLegalId      :: C.CounterValue datastore Int
  , _dbLegalEntities    :: datastore [Legal.Entity]
  , _dbNextUserId       :: C.CounterValue datastore Int
  , _dbUserProfiles     :: datastore [User.UserProfile]
  , _dbNextInvoiceId    :: C.CounterValue datastore Int
  , _dbInvoices         :: datastore [Invoice.Invoice]
  , _dbNextEmploymentId :: C.CounterValue datastore Int
  , _dbEmployments      :: datastore [Employment.Contract]

  , _dbRandomGenState     :: datastore (Word64, Word64)
    -- ^ The internal representation of a StdGen.
  , _dbFormCreateContractAll ::
      datastore (Map (User.UserName, Text) Employment.CreateContractAll)
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

  (pure $ randomGenState 42) -- Deterministic initial seed.
  (pure mempty)

instantiateStmDb
  :: forall runtime m . MonadIO m => HaskDb runtime -> m (StmDb runtime)
instantiateStmDb Db
  { _dbNextBusinessId   = CounterValue (Identity seedNextBusinessId)
  , _dbBusinessEntities = Identity seedBusinessEntities
  , _dbNextLegalId      = CounterValue (Identity seedNextLegalId)
  , _dbLegalEntities    = Identity seedLegalEntities
  , _dbNextUserId       = CounterValue (Identity seedNextUserId)
  , _dbUserProfiles     = Identity seedProfiles
  , _dbNextInvoiceId    = CounterValue (Identity seedNextInvoiceId)
  , _dbInvoices         = Identity seedInvoices
  , _dbNextEmploymentId = CounterValue (Identity seedNextEmploymentId)
  , _dbEmployments      = Identity seedEmployments

  , _dbRandomGenState        = Identity seedRandomGenState
  , _dbFormCreateContractAll = Identity seedFormCreateContractAll
  }
  =
  -- We don't use `newTVarIO` repeatedly under here and instead wrap the whole
  -- instantiation under a single STM transaction (@atomically@).
    liftIO . STM.atomically $ do
    _dbNextBusinessId   <- C.newCounter seedNextBusinessId
    _dbBusinessEntities <- STM.newTVar seedBusinessEntities
    _dbNextLegalId      <- C.newCounter seedNextLegalId
    _dbLegalEntities    <- STM.newTVar seedLegalEntities
    _dbNextUserId       <- C.newCounter seedNextUserId
    _dbUserProfiles     <- STM.newTVar seedProfiles
    _dbNextInvoiceId    <- C.newCounter seedNextInvoiceId
    _dbInvoices         <- STM.newTVar seedInvoices
    _dbNextEmploymentId <- C.newCounter seedNextEmploymentId
    _dbEmployments      <- STM.newTVar seedEmployments

    _dbRandomGenState        <- STM.newTVar seedRandomGenState
    _dbFormCreateContractAll <- STM.newTVar seedFormCreateContractAll
    pure Db { .. }

instantiateEmptyStmDb :: forall runtime m . MonadIO m => m (StmDb runtime)
instantiateEmptyStmDb = instantiateStmDb emptyHask

-- | Reset all values of the `Db` product type from `STM.STM` to the empty
-- state.
resetStmDb
  :: forall runtime m . MonadIO m => StmDb runtime -> m ()
resetStmDb stmDb = liftIO . STM.atomically $ do
  C.writeCounter (_dbNextBusinessId stmDb) seedNextBusinessId
  STM.writeTVar (_dbBusinessEntities stmDb) seedBusinessEntities
  C.writeCounter (_dbNextLegalId stmDb) seedNextLegalId
  STM.writeTVar (_dbLegalEntities stmDb) seedLegalEntities
  C.writeCounter (_dbNextUserId stmDb) seedNextUserId
  STM.writeTVar (_dbUserProfiles stmDb) seedProfiles
  C.writeCounter (_dbNextInvoiceId stmDb) seedNextInvoiceId
  STM.writeTVar (_dbInvoices stmDb) seedInvoices
  C.writeCounter (_dbNextEmploymentId stmDb) seedNextEmploymentId
  STM.writeTVar (_dbEmployments stmDb) seedEmployments

  STM.writeTVar (_dbFormCreateContractAll stmDb) seedFormCreateContractAll
 where
  Db
    { _dbNextBusinessId   = CounterValue (Identity seedNextBusinessId)
    , _dbBusinessEntities = Identity seedBusinessEntities
    , _dbNextLegalId      = CounterValue (Identity seedNextLegalId)
    , _dbLegalEntities    = Identity seedLegalEntities
    , _dbNextUserId       = CounterValue (Identity seedNextUserId)
    , _dbUserProfiles     = Identity seedProfiles
    , _dbNextInvoiceId    = CounterValue (Identity seedNextInvoiceId)
    , _dbInvoices         = Identity seedInvoices
    , _dbNextEmploymentId = CounterValue (Identity seedNextEmploymentId)
    , _dbEmployments      = Identity seedEmployments

    , _dbFormCreateContractAll = Identity seedFormCreateContractAll
    } = emptyHask

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
  _dbNextLegalId      <- pure <$> C.readCounter (_dbNextLegalId stmDb)
  _dbLegalEntities    <- pure <$> STM.readTVar (_dbLegalEntities stmDb)
  _dbNextUserId       <- pure <$> C.readCounter (_dbNextUserId stmDb)
  _dbUserProfiles     <- pure <$> STM.readTVar (_dbUserProfiles stmDb)
  _dbNextInvoiceId    <- pure <$> C.readCounter (_dbNextInvoiceId stmDb)
  _dbInvoices         <- pure <$> STM.readTVar (_dbInvoices stmDb)
  _dbNextEmploymentId <- pure <$> C.readCounter (_dbNextEmploymentId stmDb)
  _dbEmployments      <- pure <$> STM.readTVar (_dbEmployments stmDb)

  _dbRandomGenState        <- pure <$> STM.readTVar (_dbRandomGenState stmDb)
  _dbFormCreateContractAll <- pure <$> STM.readTVar (_dbFormCreateContractAll stmDb)
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


--------------------------------------------------------------------------------
-- | Write an entire db state to bytes.
serialiseDb :: forall runtime . HaskDb runtime -> LByteString
serialiseDb = encode
{-# INLINE serialiseDb #-}

-- | Read an entire db state from bytes.
deserialiseDb :: forall runtime . LByteString -> Either DbErr (HaskDb runtime)
deserialiseDb = first (DbDecodeFailed . T.pack) . eitherDecode
{-# INLINE deserialiseDb #-}


--------------------------------------------------------------------------------
-- We use System.Random.Internal and Sytem.Random.SplitMix to be able to keep
-- the random generator internal state (a pair of Word64) in our Data.StmDb
-- structure.
randomGenState :: Int -> (Word64, Word64)
randomGenState = SM.unseedSMGen . Rand.unStdGen . Rand.mkStdGen

readStdGen :: StmDb runtime -> STM Rand.StdGen
readStdGen db = do
  (seed, gamma) <- STM.readTVar $ _dbRandomGenState db
  let g = Rand.StdGen $ SM.seedSMGen' (seed, gamma)
  pure g

writeStdGen :: StmDb runtime -> Rand.StdGen -> STM ()
writeStdGen db g = do
  let (seed, gamma) = SM.unseedSMGen $ Rand.unStdGen g
  STM.writeTVar (_dbRandomGenState db) (seed, gamma)

genRandomText :: forall runtime . StmDb runtime -> STM Text
genRandomText db = do
  g1 <- readStdGen db
  let ags = take 8 $ unfoldr (\g -> let (a, g') = Rand.uniformR ('A', 'Z') g
                                     in Just ((a, g'), g')) g1
      s = T.pack $ fst <$> ags
      g2 = snd $ L.last ags
  writeStdGen db g2
  pure s
