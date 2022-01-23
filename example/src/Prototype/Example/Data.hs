{-# LANGUAGE TypeFamilies   #-}
module Prototype.Example.Data
  ( Db(..)
  , StmDb
  , HaskDb
  -- * Instantiating databases. 
  , emptyHask
  , instantiateStmDb
  , instantiateEmptyStmDb
  ) where

import qualified Control.Concurrent.STM        as STM
import qualified Prototype.Backend.InteractiveState
                                               as IS
import qualified Prototype.Example.Data.Todo   as Todo
import qualified Prototype.Example.Data.User   as U
import qualified Prototype.Runtime.Storage     as S

-- | The central database. The product type contains all values and is parameterised by @datastore@. The @datastore@ can be the layer
-- dealing with storage. When it is @Identity@, it just means the data is stored as is. It can, however, also be an `STM.TVar` if the datastore is to be
-- STM based. 
data Db (datastore :: Type -> Type) = Db
  { _dbUserProfiles :: datastore [U.UserProfile]
  , _dbTodos        :: datastore [Todo.TodoList]
  }

-- | Hask database type: used for starting the system, values reside in @Hask@ (thus `Identity`)
type HaskDb = Db Identity
-- | Stm database type, used for live example applications, values reside in @STM@  
type StmDb = Db STM.TVar

-- | Instantiate a seed database that is empty.
emptyHask :: HaskDb
emptyHask = Db (pure mempty) (pure mempty)

instantiateStmDb :: MonadIO m => HaskDb -> m StmDb
instantiateStmDb Db { _dbUserProfiles = seedProfiles, _dbTodos = seedTodos } =
  -- We don't use `newTVarIO` repeatedly under here and instead wrap the whole instantiation under a single STM transaction (@atomically@)
  liftIO . STM.atomically $ do
  -- @runIdentity@ below is necessary to unwrap the values under the @Identity@ newtype.
    _dbUserProfiles <- STM.newTVar $ runIdentity seedProfiles
    _dbTodos        <- STM.newTVar $ runIdentity seedTodos
    pure Db { .. }

instantiateEmptyStmDb :: MonadIO m => m StmDb
instantiateEmptyStmDb = instantiateStmDb emptyHask

{- | We want to represent our `StmDb` as an interactive state; and allow modifications of users and todos
it currently contains, for example. 

Some notes:

1. Modifications are pretty easy; we just enclose each value's DBUpdate's as necessary. 

2. We'd like to visualise either the users in the system, or the todo-lists, or everything.

3. For modification constraints, since we'd manifest STM operations into `IO`; we'd like to constrain on being able to
lift `IO` ops. to some arbitrary `m`

-}
instance IS.InteractiveState StmDb where
  
  data StateModification StmDb =
    ModifyUser (S.DBUpdate U.UserProfile)
    | ModifyTodo (S.DBUpdate Todo.TodoList)
  
  data StateVisualisation StmDb =
    VisualiseUser (S.DBSelect U.UserProfile)
    | VisualiseTodo (S.DBSelect Todo.TodoList)
    | VisualiseFullStmDb

  type StateModificationC StmDb m = (MonadIO m)
  type StateVisualisationC StmDb m = (MonadIO m)
  
  data StateModificationOutput StmDb = UsersModified [U.UserProfile]
                                     | TodoListsModified [Todo.TodoList]
