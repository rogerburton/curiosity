{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies   #-}
{- |
Module: Curiosity.Data
Description: This module defines the central state data type.

-}

-- brittany-disable-next-binding
module Curiosity.Data
  ( Db(..)
  , HaskDb
  -- * Instantiating databases.
  , emptyHask
  -- * Serialising and deseralising DB to bytes.
  , serialiseDb
  , deserialiseDb
  , deserialiseDbStrict
  -- * Re-exports
  , Command.Command(..)
  ) where

import qualified Commence.Runtime.Errors       as E
import qualified Curiosity.Data.Business       as Business
import qualified Curiosity.Data.Command        as Command
                                                ( Command(..) )
import qualified Curiosity.Data.Counter        as C
import qualified Curiosity.Data.Email          as Email
import qualified Curiosity.Data.Employment     as Employment
import qualified Curiosity.Data.Invoice        as Invoice
import qualified Curiosity.Data.Legal          as Legal
import qualified Curiosity.Data.Order          as Order
import qualified Curiosity.Data.Quotation      as Quotation
import qualified Curiosity.Data.RemittanceAdv  as RemittanceAdv
import qualified Curiosity.Data.SimpleContract as SimpleContract
import qualified Curiosity.Data.User           as User
import           Data.Aeson
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
-- brittany-disable-next-binding
data Db (datastore :: Type -> Type) = Db
  { _dbNextBusinessId   :: C.CounterValue datastore Int
  , _dbBusinessUnits    :: datastore [Business.Unit]
  , _dbNextLegalId      :: C.CounterValue datastore Int
  , _dbLegalEntities    :: datastore [Legal.Entity]
  , _dbNextUserId       :: C.CounterValue datastore Int
  , _dbUserProfiles     :: datastore [User.UserProfile]
  , _dbNextQuotationId  :: C.CounterValue datastore Int
  , _dbQuotations       :: datastore [Quotation.Quotation]
  , _dbNextOrderId      :: C.CounterValue datastore Int
  , _dbOrders           :: datastore [Order.Order]
  , _dbNextInvoiceId    :: C.CounterValue datastore Int
  , _dbInvoices         :: datastore [Invoice.Invoice]
  , _dbNextRemittanceAdvId :: C.CounterValue datastore Int
  , _dbRemittanceAdvs      :: datastore [RemittanceAdv.RemittanceAdv]
  , _dbNextEmploymentId :: C.CounterValue datastore Int
  , _dbEmployments      :: datastore [Employment.Contract]

  , _dbRandomGenState   :: datastore (Word64, Word64)
    -- ^ The internal representation of a StdGen.

  , _dbFormCreateQuotationAll ::
      datastore (Map (User.UserName, Text) Quotation.CreateQuotationAll)
  , _dbFormCreateContractAll ::
      datastore (Map (User.UserName, Text) Employment.CreateContractAll)
  , _dbFormCreateSimpleContractAll ::
      datastore (Map (User.UserName, Text) SimpleContract.CreateContractAll)

  , _dbNextEmailId :: C.CounterValue datastore Int
  , _dbEmails      :: datastore [Email.Email]
  }

-- | Hask database type: used for starting the system, values reside in @Hask@
-- (thus `Identity`)
type HaskDb = Db Identity

deriving instance Eq HaskDb
deriving instance Show HaskDb
deriving instance Generic HaskDb
deriving anyclass instance ToJSON HaskDb
deriving anyclass instance FromJSON HaskDb

-- | Instantiate a seed database that is empty.
emptyHask :: HaskDb
emptyHask = Db (pure 1)
               (pure mempty)
               (pure 1)
               (pure mempty)
               (pure 1)
               (pure mempty)
               (pure 1)
               (pure mempty)
               (pure 1)
               (pure mempty)
               (pure 1)
               (pure mempty)
               (pure 1)
               (pure mempty)
               (pure 1)
               (pure mempty)

               (pure initialGenState)
               (pure mempty)
               (pure mempty)
               (pure mempty)

               (pure 1)
               (pure mempty)

initialGenState :: (Word64, Word64)
initialGenState = randomGenState 42 -- Deterministic initial seed.

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
serialiseDb :: HaskDb -> LByteString
serialiseDb = encode
{-# INLINE serialiseDb #-}

-- | Read an entire db state from bytes.
deserialiseDb :: LByteString -> Either DbErr HaskDb
deserialiseDb = first (DbDecodeFailed . T.pack) . eitherDecode
{-# INLINE deserialiseDb #-}

-- | Read an entire db state from bytes.
deserialiseDbStrict :: ByteString -> Either DbErr HaskDb
deserialiseDbStrict = first (DbDecodeFailed . T.pack) . eitherDecodeStrict
{-# INLINE deserialiseDbStrict #-}


--------------------------------------------------------------------------------
-- We use System.Random.Internal and Sytem.Random.SplitMix to be able to keep
-- the random generator internal state (a pair of Word64) in our Data.StmDb
-- structure.
randomGenState :: Int -> (Word64, Word64)
randomGenState = SM.unseedSMGen . Rand.unStdGen . Rand.mkStdGen
