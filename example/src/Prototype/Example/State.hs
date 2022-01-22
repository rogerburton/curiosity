module Prototype.Example.State
  () where

import qualified Control.Concurrent.STM.TVar   as STM

data Todo
data UserProfile

data ExampleState = ExampleState
  { _esTodos       :: STM.TVar [Todo] -- ^ Current Todo items in the state
  , _esUserProfile :: UserProfile
  }
