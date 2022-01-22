module Prototype.Example.State
  () where

import qualified Control.Concurrent.STM.TVar   as STM
import qualified Prototype.Example.Data.User   as U

data Todo

data ExampleState = ExampleState
  { _esTodos        :: STM.TVar [Todo] -- ^ Current Todo items in the state
  , _esUserProfiles :: STM.TVar [U.UserProfile] -- ^ Current user profiles. 
  }
