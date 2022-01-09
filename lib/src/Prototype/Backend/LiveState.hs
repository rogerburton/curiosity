{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds      #-}
{- |
Module: Prototype.Backend.LiveState
Description: Live state server
-}
module Prototype.Backend.LiveState
  () where

import           Brick

-- | The output medium for the live state. 
data LiveStateOutput = Brick
  deriving Show

{- |
Backend's live state visualisation.

Usually, @storage@ is the product type containing STM based values you'd like to output into an @output@.

At the time of writing, we only support the Brick terminal output. 

In the future, we'd also like to support additional modes like a @Repl@ etc. 
-}
class LiveState storage (output :: LiveStateOutput) where


