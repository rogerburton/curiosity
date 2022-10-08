{-# LANGUAGE TemplateHaskell #-}
-- | STM operations around `Curiosity.Data`.

-- brittany-disable-next-binding
module Curiosity.Core
  ( reset
  , createUser
  , createUserFull
  , modifyUsers
  , selectUserById
  , selectUserByUsername
  -- * ID generation
  , generateUserId
  , generateBusinessId
  , generateLegalId
  , generateQuotationId
  , generateOrderId
  , generateRemittanceAdvId
  , generateEmploymentId
  , generateInvoiceId
  , generateEmailId
  , firstUserId
  -- * Operations on business units
  , createBusiness
  , updateBusiness
  , selectUnitBySlug
  -- * User rights
  , canPerform
  ) where

import qualified Control.Concurrent.STM        as STM
import           Control.Lens
import           Curiosity.Data
import qualified Curiosity.Data.Business       as Business
import qualified Curiosity.Data.Counter        as C
import qualified Curiosity.Data.Email          as Email
import qualified Curiosity.Data.Employment     as Employment
import qualified Curiosity.Data.Invoice        as Invoice
import qualified Curiosity.Data.Legal          as Legal
import qualified Curiosity.Data.Order          as Order
import qualified Curiosity.Data.Quotation      as Quotation
import qualified Curiosity.Data.RemittanceAdv  as RemittanceAdv
import qualified Curiosity.Data.User           as User
import qualified Language.Haskell.TH.Syntax    as Syntax


--------------------------------------------------------------------------------
-- brittany-disable-next-binding
reset :: StmDb runtime -> STM ()
reset stmDb = do
  C.writeCounter (_dbNextBusinessId stmDb) seedNextBusinessId
  STM.writeTVar (_dbBusinessUnits stmDb) seedBusinessUnits
  C.writeCounter (_dbNextLegalId stmDb) seedNextLegalId
  STM.writeTVar (_dbLegalEntities stmDb) seedLegalEntities
  C.writeCounter (_dbNextUserId stmDb) seedNextUserId
  STM.writeTVar (_dbUserProfiles stmDb) seedProfiles
  C.writeCounter (_dbNextQuotationId stmDb) seedNextQuotationId
  STM.writeTVar (_dbQuotations stmDb) seedQuotations
  C.writeCounter (_dbNextOrderId stmDb) seedNextOrderId
  STM.writeTVar (_dbOrders stmDb) seedOrders
  C.writeCounter (_dbNextInvoiceId stmDb) seedNextInvoiceId
  STM.writeTVar (_dbInvoices stmDb) seedInvoices
  C.writeCounter (_dbNextRemittanceAdvId stmDb) seedNextRemittanceAdvId
  STM.writeTVar (_dbRemittanceAdvs stmDb) seedRemittanceAdvs
  C.writeCounter (_dbNextEmploymentId stmDb) seedNextEmploymentId
  STM.writeTVar (_dbEmployments stmDb) seedEmployments

  STM.writeTVar (_dbRandomGenState stmDb) seedRandomGenState
  STM.writeTVar (_dbFormCreateQuotationAll stmDb) seedFormCreateQuotationAll
  STM.writeTVar (_dbFormCreateContractAll stmDb) seedFormCreateContractAll
  STM.writeTVar (_dbFormCreateSimpleContractAll stmDb)
                seedFormCreateSimpleContractAll

  C.writeCounter (_dbNextEmailId stmDb) seedNextEmailId
  STM.writeTVar (_dbEmails stmDb) seedEmails
 where
  Db
    { _dbNextBusinessId = C.CounterValue (Identity seedNextBusinessId)
    , _dbBusinessUnits = Identity seedBusinessUnits
    , _dbNextLegalId = C.CounterValue (Identity seedNextLegalId)
    , _dbLegalEntities = Identity seedLegalEntities
    , _dbNextUserId = C.CounterValue (Identity seedNextUserId)
    , _dbUserProfiles = Identity seedProfiles
    , _dbNextQuotationId = C.CounterValue (Identity seedNextQuotationId)
    , _dbQuotations = Identity seedQuotations
    , _dbNextOrderId = C.CounterValue (Identity seedNextOrderId)
    , _dbOrders = Identity seedOrders
    , _dbNextInvoiceId = C.CounterValue (Identity seedNextInvoiceId)
    , _dbInvoices = Identity seedInvoices
    , _dbNextRemittanceAdvId = C.CounterValue (Identity seedNextRemittanceAdvId)
    , _dbRemittanceAdvs = Identity seedRemittanceAdvs
    , _dbNextEmploymentId = C.CounterValue (Identity seedNextEmploymentId)
    , _dbEmployments = Identity seedEmployments
    , _dbRandomGenState = Identity seedRandomGenState
    , _dbFormCreateQuotationAll = Identity seedFormCreateQuotationAll
    , _dbFormCreateContractAll = Identity seedFormCreateContractAll
    , _dbFormCreateSimpleContractAll = Identity seedFormCreateSimpleContractAll
    , _dbNextEmailId = C.CounterValue (Identity seedNextEmailId)
    , _dbEmails = Identity seedEmails
    }
    = emptyHask


--------------------------------------------------------------------------------
generateUserId :: forall runtime . StmDb runtime -> STM User.UserId
generateUserId Db {..} =
  User.UserId <$> C.bumpCounterPrefix User.userIdPrefix _dbNextUserId

generateBusinessId
  :: forall runtime . StmDb runtime -> STM Business.UnitId
generateBusinessId Db {..} =
  Business.UnitId
    <$> C.bumpCounterPrefix Business.unitIdPrefix _dbNextBusinessId

generateLegalId :: forall runtime . StmDb runtime -> STM Legal.EntityId
generateLegalId Db {..} =
  Legal.EntityId <$> C.bumpCounterPrefix Legal.entityIdPrefix _dbNextLegalId

generateQuotationId
  :: forall runtime . StmDb runtime -> STM Quotation.QuotationId
generateQuotationId Db {..} =
  Quotation.QuotationId
    <$> C.bumpCounterPrefix Quotation.quotationIdPrefix _dbNextQuotationId

generateOrderId :: forall runtime . StmDb runtime -> STM Order.OrderId
generateOrderId Db {..} =
  Order.OrderId <$> C.bumpCounterPrefix Order.orderIdPrefix _dbNextOrderId

generateRemittanceAdvId
  :: forall runtime . StmDb runtime -> STM RemittanceAdv.RemittanceAdvId
generateRemittanceAdvId Db {..} =
  RemittanceAdv.RemittanceAdvId
    <$> C.bumpCounterPrefix RemittanceAdv.remittanceAdvIdPrefix
                            _dbNextRemittanceAdvId

generateEmploymentId
  :: forall runtime . StmDb runtime -> STM Employment.ContractId
generateEmploymentId Db {..} =
  Employment.ContractId
    <$> C.bumpCounterPrefix Employment.contractIdPrefix _dbNextEmploymentId

generateInvoiceId
  :: forall runtime . StmDb runtime -> STM Invoice.InvoiceId
generateInvoiceId Db {..} =
  Invoice.InvoiceId
    <$> C.bumpCounterPrefix Invoice.invoiceIdPrefix _dbNextInvoiceId

generateEmailId :: forall runtime . StmDb runtime -> STM Email.EmailId
generateEmailId Db {..} =
  Email.EmailId <$> C.bumpCounterPrefix Email.emailIdPrefix _dbNextEmailId


--------------------------------------------------------------------------------
firstUserId :: User.UserId
firstUserId = User.UserId $ User.userIdPrefix <> "1"

firstUserRights :: [User.AccessRight]
firstUserRights = [User.CanCreateContracts, User.CanVerifyEmailAddr]


--------------------------------------------------------------------------------
createUser
  :: forall runtime
   . StmDb runtime
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
   . StmDb runtime
  -> User.UserProfile
  -> STM (Either User.UserErr User.UserId)
createUserFull db newProfile = if username `elem` User.usernameBlocklist
  then pure . Left $ User.UsernameBlocked
  else do
    mprofile <- selectUserById db newProfileId
    case mprofile of
      Just _  -> existsErr
      Nothing -> createNew
 where
  username     = newProfile ^. User.userProfileCreds . User.userCredsName
  newProfileId = User._userProfileId newProfile
  createNew    = do
    mprofile <- selectUserByUsername db username
    case mprofile of
      Just _  -> existsErr
      Nothing -> do
        modifyUsers db (++ [newProfile])
        pure $ Right newProfileId
  existsErr = pure . Left $ User.UserExists

modifyUsers
  :: forall runtime
   . StmDb runtime
  -> ([User.UserProfile] -> [User.UserProfile])
  -> STM ()
modifyUsers db f =
  let usersTVar = _dbUserProfiles db in STM.modifyTVar usersTVar f

selectUserById db id = do
  let usersTVar = _dbUserProfiles db
  STM.readTVar usersTVar <&> find ((== id) . User._userProfileId)

selectUserByUsername
  :: forall runtime
   . StmDb runtime
  -> User.UserName
  -> STM (Maybe User.UserProfile)
selectUserByUsername db username = do
  let usersTVar = _dbUserProfiles db
  users' <- STM.readTVar usersTVar
  pure $ find ((== username) . User._userCredsName . User._userProfileCreds)
              users'


--------------------------------------------------------------------------------
createBusiness
  :: forall runtime
   . StmDb runtime
  -> Business.Create
  -> STM (Either Business.Err Business.UnitId)
createBusiness db Business.Create {..} = do
  STM.catchSTM (Right <$> transaction) (pure . Left)
 where
  transaction = do
    newId <- generateBusinessId db
    let new = Business.Unit newId _createSlug _createName Nothing
    createBusinessFull db new >>= either STM.throwSTM pure

createBusinessFull
  :: forall runtime
   . StmDb runtime
  -> Business.Unit
  -> STM (Either Business.Err Business.UnitId)
createBusinessFull db new = do
  modifyBusinessUnits db (++ [new])
  pure . Right $ Business._entityId new

updateBusiness
  :: forall runtime
   . StmDb runtime
  -> Business.Update
  -> STM (Either Business.Err ())
updateBusiness db Business.Update {..} = do
  mentity <- selectUnitBySlug db _updateSlug
  case mentity of
    Just Business.Unit{} -> do
      let replaceOlder entities =
            [ if Business._entitySlug e == _updateSlug
                then e { Business._entityDescription = _updateDescription }
                else e
            | e <- entities
            ]
      modifyBusinessUnits db replaceOlder
      pure $ Right ()
    Nothing ->
      pure . Left . Business.Err $ "No such business unit: " <> _updateSlug

modifyBusinessUnits
  :: forall runtime
   . StmDb runtime
  -> ([Business.Unit] -> [Business.Unit])
  -> STM ()
modifyBusinessUnits db f =
  let tvar = _dbBusinessUnits db in STM.modifyTVar tvar f

selectUnitBySlug
  :: forall runtime . StmDb runtime -> Text -> STM (Maybe Business.Unit)
selectUnitBySlug db name = do
  let tvar = _dbBusinessUnits db
  records <- STM.readTVar tvar
  pure $ find ((== name) . Business._entitySlug) records


--------------------------------------------------------------------------------
canPerform :: Syntax.Name -> StmDb runtime -> User.UserProfile -> STM Bool
canPerform action _ User.UserProfile {..}
  | action == 'User.SetUserEmailAddrAsVerified
  = pure $ User.CanVerifyEmailAddr `elem` _userProfileRights
  | otherwise
  = pure False
