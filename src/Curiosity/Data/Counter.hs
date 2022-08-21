{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures #-}
module Curiosity.Data.Counter
  ( CounterValue(..)
  , Counter(..)
  , currentCounterBumping
  ) where

import qualified Control.Concurrent.STM        as STM
import           Data.Aeson

newtype CounterValue (datastore :: Type -> Type) count = CounterValue { counterValue :: datastore count }
                                                       deriving (Eq, Show)
                                                       deriving (ToJSON, FromJSON) via datastore count

instance Functor datastore => Functor (CounterValue datastore) where
  fmap f (CounterValue dcount) = CounterValue $ fmap f dcount
instance Applicative datastore => Applicative (CounterValue datastore) where
  pure = CounterValue . pure
  liftA2 f (CounterValue dcount0) (CounterValue dcount1) =
    CounterValue $ liftA2 f dcount0 dcount1

class Counter count datastore m where

  -- | Generate the initial value of a counter.
  initialCounter :: count ->  m (CounterValue datastore count)

  -- | Generate the next value of a counter.
  currentCounter :: CounterValue datastore count -> m count

  -- | Store the new counter value. 
  bumpCounter :: CounterValue datastore count -> m ()

instance Enum count => Counter count STM.TVar STM.STM  where

  initialCounter initCount = CounterValue <$> STM.newTVar initCount

  currentCounter (CounterValue countTvar) = STM.readTVar countTvar

  bumpCounter (CounterValue countTvar) = STM.modifyTVar countTvar succ

-- | Since we're dealing with pure values, bumpCounter has no effect. 
instance Enum count => Counter count Identity Identity where
  initialCounter initCount = pure $ CounterValue . Identity $ initCount
  currentCounter (CounterValue (Identity curValue)) = pure curValue
  bumpCounter (CounterValue (Identity curValue)) =
    pure (CounterValue (Identity $ succ curValue)) $> ()

-- | Get the current value of a counter bumping the value as we go.
currentCounterBumping
  :: forall count m datastore
   . (Counter count datastore m, Applicative m)
  => CounterValue datastore count
  -> m count
currentCounterBumping ctr = currentCounter ctr <* bumpCounter ctr

