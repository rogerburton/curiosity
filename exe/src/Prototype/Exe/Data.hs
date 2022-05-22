{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies   #-}
module Prototype.Exe.Data
  ( Db(..)
  , StmDb
  , HaskDb
  -- * Constraints
  , RuntimeHasStmDb(..)
  -- * Instantiating databases. 
  , emptyHask
  , instantiateStmDb
  , instantiateEmptyStmDb
  -- * Reading values from the database.
  , readFullStmDbInHask
  , IS.StateModification(..)
  , IS.InteractiveStateErr(..)
  -- * typeclass-free parsers
  , parseViz
  , parseMod
  ) where

import qualified Control.Concurrent.STM        as STM
import qualified Prototype.Backend.InteractiveState.Class
                                               as IS
import qualified Prototype.Exe.Data.Todo       as Todo
import qualified Prototype.Exe.Data.User       as U
import qualified Prototype.Exe.Repl.Parse      as P
import qualified Prototype.Runtime.Errors      as Errs
import qualified Prototype.Runtime.Storage     as S

{- | The central database. The product type contains all values and is parameterised by @datastore@. The @datastore@ can be the layer
dealing with storage. When it is @Identity@, it just means the data is stored as is. It can, however, also be an `STM.TVar` if the datastore is to be
STM based. 

Additionally, we want to parameterise over a @runtime@ type parameter. This is a container type of the database. 
-}
data Db (datastore :: Type -> Type) (runtime :: Type) = Db
  { _dbUserProfiles :: datastore [U.UserProfile]
  , _dbTodos        :: datastore [Todo.TodoList]
  }

-- | Hask database type: used for starting the system, values reside in @Hask@ (thus `Identity`)
type HaskDb = Db Identity

deriving instance Show (HaskDb runtime)

-- | Stm database type, used for live example applications, values reside in @STM@  
type StmDb = Db STM.TVar

-- | Instantiate a seed database that is empty.
emptyHask :: forall runtime . HaskDb runtime
emptyHask = Db (pure mempty) (pure mempty)

instantiateStmDb
  :: forall runtime m . MonadIO m => HaskDb runtime -> m (StmDb runtime)
instantiateStmDb Db { _dbUserProfiles = seedProfiles, _dbTodos = seedTodos } =
  -- We don't use `newTVarIO` repeatedly under here and instead wrap the whole instantiation under a single STM transaction (@atomically@)
  liftIO . STM.atomically $ do
  -- @runIdentity@ below is necessary to unwrap the values under the @Identity@ newtype.
    _dbUserProfiles <- STM.newTVar $ runIdentity seedProfiles
    _dbTodos        <- STM.newTVar $ runIdentity seedTodos
    pure Db { .. }

instantiateEmptyStmDb :: forall runtime m . MonadIO m => m (StmDb runtime)
instantiateEmptyStmDb = instantiateStmDb emptyHask

-- | Reads all values of the `Db` product type from `STM.STM` to @Hask@.
readFullStmDbInHask
  :: forall runtime m
   . (MonadIO m, RuntimeHasStmDb runtime)
  => runtime
  -> m (HaskDb runtime)
readFullStmDbInHask runtime =
  let stmDb = stmDbFromRuntime runtime
  in  liftIO . STM.atomically $ do
        _dbUserProfiles <- pure <$> STM.readTVar (_dbUserProfiles stmDb)
        _dbTodos        <- pure <$> STM.readTVar (_dbTodos stmDb)
        pure Db { .. }

{- | Provides us with the ability to constrain on a larger product-type (the @runtime@) to contain, in some form or another, a value
of the `StmDb`, which can be accessed from the @runtime@.

This solves cyclic imports, without caring about the concrete @runtime@ types, we can just rely on the constraints. 
-}
class RuntimeHasStmDb runtime where
  stmDbFromRuntime :: runtime -> StmDb runtime

{- | We want to represent our `StmDb` as an interactive state; and allow modifications of users and todos
it currently contains, for example. 

Some notes:

1. Modifications are pretty easy; we just enclose each value's DBUpdate's as necessary. 

2. We'd like to visualise either the users in the system, or the todo-lists, or everything.

3. For modification constraints, since we'd manifest STM operations into `IO`; we'd like to constrain on being able to
lift `IO` ops. to some arbitrary `m`

-}
instance IS.InteractiveState (StmDb runtime) where
  
  data StateModification (StmDb runtime) =
    ModifyUser (S.DBUpdate U.UserProfile)
    | ModifyTodo (S.DBUpdate Todo.TodoList)
      deriving Show
  
  data InteractiveStateErr (StmDb runtime) = ParseFailed P.ParseErr
      deriving Show
  
  data StateVisualisation (StmDb runtime) =
    VisualiseUser (S.DBSelect U.UserProfile)
    | VisualiseTodo (S.DBSelect Todo.TodoList)
    | VisualiseFullStmDb
      deriving Show

  type StateModificationC (StmDb runtime) m
    = ( MonadIO m
      , MonadError Errs.RuntimeErr m
      , MonadReader runtime m
      , RuntimeHasStmDb runtime
      -- The constraints that actually let us perform the updates
      -- within this @m@.
      , S.DBStorage m U.UserProfile
      , S.DBStorage m Todo.TodoList
      )
  type StateVisualisationC (StmDb runtime) m = IS.StateModificationC
    (StmDb runtime)
    m
  
  data StateModificationOutput (StmDb runtime) = UsersModified [U.UserProfile]
                                               | TodoListsModified [Todo.TodoList]
                                               deriving Show
  
  data StateVisualisationOutput (StmDb runtime) = UsersVisualised [U.UserProfile]
                                                | TodoListsVisualised [Todo.TodoList]
                                                | FullStmDbVisualised (HaskDb runtime)
                                                deriving Show

  execVisualisation = \case
    VisualiseUser userSelect -> S.dbSelect userSelect <&> UsersVisualised
    VisualiseTodo todoSelect -> S.dbSelect todoSelect <&> TodoListsVisualised
    VisualiseFullStmDb -> ask >>= readFullStmDbInHask <&> FullStmDbVisualised

  execModification = \case
    ModifyUser userUpdate -> S.dbUpdate userUpdate >>= getAffectedUsers
     where
      getAffectedUsers =
        fmap UsersModified . concatMapM (S.dbSelect . U.SelectUserById)

    ModifyTodo todoUpdate -> S.dbUpdate todoUpdate >>= getAffectedTodos
     where
      getAffectedTodos = fmap TodoListsModified
        . concatMapM (S.dbSelect . Todo.SelectTodoListById)

{- | This instance defines parsing for incoming inputs that visualise or modify the `Db` state.. 

All modification inputs need to start with the word @mod@ (case insensitive)

For example, to modfify a user, we should have commands of the form:

@
mod user <UserSubcommand>
@

etc.
-}
instance IS.InteractiveStateOnDisp (StmDb runtime) 'IS.Repl where

  type StateParseInputC (StmDb runtime) 'IS.Repl m = (Applicative m)

  parseModificationInput
    :: forall m
     . (IS.StateParseInputC (StmDb runtime) 'IS.Repl m)
    => IS.DispInput 'IS.Repl -- ^ Raw input 
    -> m
         ( Either
             (IS.InteractiveStateErr (StmDb runtime))
             (IS.StateModification (StmDb runtime))
         ) -- ^ We eventually return a successfully parsed modification.
  parseModificationInput (IS.ReplInputStrict input) =
    pure $ parseMod input & first ParseFailed

  parseVisualisationInput (IS.ReplInputStrict input) =
    pure $ parseViz input & first ParseFailed

parseViz
  :: forall runtime
   . Text
  -> Either P.ParseErr (IS.StateVisualisation (StmDb runtime))
parseViz =
  P.parseInputCtx
    $  P.withTrailSpaces "viz"
    *> (let
          userViz =
            P.withTrailSpaces "user" *> U.dbSelectParser <&> VisualiseUser
          todoViz =
            P.withTrailSpaces "todo" *> Todo.dbSelectParser <&> VisualiseTodo
          fullViz = P.string' "all" $> VisualiseFullStmDb
        in
          P.tryAlts [userViz, todoViz, fullViz]
       )

parseMod
  :: forall runtime
   . Text
  -> Either P.ParseErr (IS.StateModification (StmDb runtime))
parseMod =
  P.parseInputCtx
    $  P.withTrailSpaces "mod"
    *> (let userMod =
              P.withTrailSpaces "user" *> U.dbUpdateParser <&> ModifyUser
            todoMod =
              P.withTrailSpaces "todo" *> Todo.dbUpdateParser <&> ModifyTodo
        in  P.tryAlts [todoMod, userMod]
       )
