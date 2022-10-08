{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
-- An attempt at creating a DSL to make it easy to play with the high-level
-- operations provided by Curiosity.
-- In the future this might be useful to try to express complex business rules
-- clearly, possibly for non-developers.
module Curiosity.Dsl
  ( Run(..)
  , run
  , reset
  , db0
  , state
  , user
  , signup
  , can
  , example
  ) where

import qualified Control.Concurrent.STM        as STM
import qualified Curiosity.Core                as Core
import qualified Curiosity.Data                as Data
import           Curiosity.Data                 ( HaskDb
                                                , readFullStmDbInHask'
                                                )
import qualified Curiosity.Data.User           as User
import qualified Language.Haskell.TH.Syntax    as Syntax
import           Prelude                 hiding ( state )


--------------------------------------------------------------------------------
newtype Run a = Run { runM :: ReaderT (Data.StmDb ()) STM a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader (Data.StmDb ())
           )

-- Is it possible to implement MonadFail only when the return type is Either ?
-- deriving instance MonadFail (Run (Either Text a))

run :: forall a . HaskDb () -> Run a -> IO a
run db Run {..} = do
  db' <- Data.instantiateStmDb db
  STM.atomically $ runReaderT runM db'


--------------------------------------------------------------------------------
db0 = Data.emptyHask

state :: Run (HaskDb ())
state = ask >>= (Run . lift . readFullStmDbInHask')

reset :: Run ()
reset = ask >>= (Run . lift . Core.reset)

user :: User.UserName -> Run (Maybe User.UserProfile)
user username = ask >>= (Run . lift . flip Core.selectUserByUsername username)

signup
  :: User.UserName
  -> User.Password
  -> User.UserEmailAddr
  -> Run (Either User.UserErr User.UserId)
signup username password email =
  ask >>= (Run . lift . flip Core.createUser input)
  where input = User.Signup username password email True

can :: User.UserProfile -> Syntax.Name -> Run Bool
can profile name = ask >>= (Run . lift . flip (Core.canPerform name) profile)


--------------------------------------------------------------------------------
-- In a GHCi session, e.g. obtained with scripts/ghci-dsl.sh:
--     ghci> run db0 example
example :: Run (Bool, Bool)
example = do
  signup "alice" "a" "alice@example.com"
  signup "bob"   "b" "bob@example.com"

  mprofilea <- user "alice"
  a         <- case mprofilea of
    Just profile -> profile `can` 'User.SetUserEmailAddrAsVerified
    Nothing      -> pure False

  mprofileb <- user "bob"
  b         <- case mprofileb of
    Just profile -> profile `can` 'User.SetUserEmailAddrAsVerified
    Nothing      -> pure False

  return (a, b)
