{-# LANGUAGE TemplateHaskell #-}
-- | STM operations around `Curiosity.Data`.
module Curiosity.Core
  ( reset
  , createUser
  , createUserFull
  , modifyUsers
  , selectUserById
  , selectUserByUsername
  , canPerform
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
  ) where

import qualified Control.Concurrent.STM        as STM
import           Control.Lens
import qualified Curiosity.Data                as Data
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
type StmDb runtime = Data.Db STM.TVar runtime

reset :: StmDb runtime -> STM ()
reset = Data.resetStmDb'


--------------------------------------------------------------------------------
generateUserId :: forall runtime . Data.StmDb runtime -> STM User.UserId
generateUserId Data.Db {..} =
  User.UserId <$> C.bumpCounterPrefix User.userIdPrefix _dbNextUserId

generateBusinessId
  :: forall runtime . Data.StmDb runtime -> STM Business.UnitId
generateBusinessId Data.Db {..} =
  Business.UnitId <$> C.bumpCounterPrefix "BENT-" _dbNextBusinessId

generateLegalId :: forall runtime . Data.StmDb runtime -> STM Legal.EntityId
generateLegalId Data.Db {..} =
  Legal.EntityId <$> C.bumpCounterPrefix "LENT-" _dbNextLegalId

generateQuotationId
  :: forall runtime . Data.StmDb runtime -> STM Quotation.QuotationId
generateQuotationId Data.Db {..} =
  Quotation.QuotationId <$> C.bumpCounterPrefix "QUOT-" _dbNextQuotationId

generateOrderId :: forall runtime . Data.StmDb runtime -> STM Order.OrderId
generateOrderId Data.Db {..} =
  Order.OrderId <$> C.bumpCounterPrefix "ORD-" _dbNextOrderId

generateRemittanceAdvId
  :: forall runtime . Data.StmDb runtime -> STM RemittanceAdv.RemittanceAdvId
generateRemittanceAdvId Data.Db {..} =
  RemittanceAdv.RemittanceAdvId
    <$> C.bumpCounterPrefix "REM-" _dbNextRemittanceAdvId

generateEmploymentId
  :: forall runtime . Data.StmDb runtime -> STM Employment.ContractId
generateEmploymentId Data.Db {..} =
  Employment.ContractId <$> C.bumpCounterPrefix "EMP-" _dbNextEmploymentId

generateInvoiceId
  :: forall runtime . Data.StmDb runtime -> STM Invoice.InvoiceId
generateInvoiceId Data.Db {..} =
  Invoice.InvoiceId <$> C.bumpCounterPrefix "INV-" _dbNextInvoiceId

generateEmailId :: forall runtime . Data.StmDb runtime -> STM Email.EmailId
generateEmailId Data.Db {..} =
  Email.EmailId <$> C.bumpCounterPrefix "EMAIL-" _dbNextEmailId


--------------------------------------------------------------------------------
firstUserId :: User.UserId
firstUserId = User.UserId $ User.userIdPrefix <> "1"

firstUserRights :: [User.AccessRight]
firstUserRights = [User.CanCreateContracts, User.CanVerifyEmailAddr]


--------------------------------------------------------------------------------
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
   . Data.StmDb runtime
  -> ([User.UserProfile] -> [User.UserProfile])
  -> STM ()
modifyUsers db f =
  let usersTVar = Data._dbUserProfiles db in STM.modifyTVar usersTVar f

selectUserById db id = do
  let usersTVar = Data._dbUserProfiles db
  STM.readTVar usersTVar <&> find ((== id) . User._userProfileId)

selectUserByUsername
  :: forall runtime
   . StmDb runtime
  -> User.UserName
  -> STM (Maybe User.UserProfile)
selectUserByUsername db username = do
  let usersTVar = Data._dbUserProfiles db
  users' <- STM.readTVar usersTVar
  pure $ find ((== username) . User._userCredsName . User._userProfileCreds)
              users'


--------------------------------------------------------------------------------
canPerform :: Syntax.Name -> Data.StmDb runtime -> User.UserProfile -> STM Bool
canPerform action _ User.UserProfile {..}
  | action == 'User.SetUserEmailAddrAsVerified
  = pure $ User.CanVerifyEmailAddr `elem` _userProfileRights
  | otherwise
  = pure False
