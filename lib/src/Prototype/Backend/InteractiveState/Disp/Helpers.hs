{- |
Module: Prototype.Backend.InteractiveState.Disp.Helpers
Description: Display helpers

-}
module Prototype.Backend.InteractiveState.Disp.Helpers
  ( PShow
  ) where

import qualified Data.Text.Lazy                as T
import qualified GHC.Show                      as Show
import           Protolude
import qualified Text.Pretty.Simple            as PS

-- | A newtype wrapper that can be used with @DerivingVia@ to derive pretty show instances for types. 
newtype PShow a = PShow { unpackPShow :: a }

instance Show.Show a => Show.Show (PShow a) where
  show = T.unpack . PS.pShow . unpackPShow
