{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
-- An attempt at creating a DSL to make it easy to play with the high-level
-- operations provided by Curiosity.
-- In the future this might be useful to try to express complex business rules
-- clearly, possibly for non-developers.
module Curiosity.Dsl where

import qualified Control.Concurrent.STM        as STM
import qualified Curiosity.Data                as Data
import           Curiosity.Data                 ( HaskDb
                                                , readFullStmDbInHask'
                                                )
import qualified Curiosity.Data.User           as User
import qualified Curiosity.Runtime             as Rt
import qualified Language.Haskell.TH.Syntax    as Syntax
import           Prelude                 hiding ( state )


--------------------------------------------------------------------------------
newtype Run a = Run { runM :: ReaderT (Data.StmDb Rt.Runtime) STM a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader (Data.StmDb Rt.Runtime)
           )

-- Is it possible to implement MonadFail only when the return type is Either ?
-- deriving instance MonadFail (Run (Either Text a))

run :: forall a . HaskDb Rt.Runtime -> Run a -> IO a
run db Run {..} = do
  db' <- Data.instantiateStmDb db
  STM.atomically $ runReaderT runM db'


--------------------------------------------------------------------------------
db0 = Data.emptyHask

state :: Run (HaskDb Rt.Runtime)
state = ask >>= (Run . lift . readFullStmDbInHask')

user :: User.UserName -> Run (Maybe User.UserProfile)
user username = ask >>= (Run . lift . flip Rt.selectUserByUsername username)

signup
  :: User.UserName
  -> User.Password
  -> User.UserEmailAddr
  -> Run (Either User.UserErr User.UserId)
signup username password email =
  ask >>= (Run . lift . flip Rt.createUser input)
  where input = User.Signup username password email True

can :: User.UserProfile -> Syntax.Name -> Run Bool
can profile name = ask >>= (Run . lift . flip (Rt.canPerform name) profile)


--------------------------------------------------------------------------------
example :: Run (Bool, Bool)
example = do
  signup "alice" "a" "alice@example.com"
  signup "bob"   "b" "bob@example.com"

  mprofile <- user "alice"
  a        <- case mprofile of
    Just profile -> profile `can` 'User.SetUserEmailAddrAsVerified
    Nothing      -> pure False

  mprofile <- user "bob"
  b        <- case mprofile of
    Just profile -> profile `can` 'User.SetUserEmailAddrAsVerified
    Nothing      -> pure False

  return (a, b)
