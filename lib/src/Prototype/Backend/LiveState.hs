{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds      #-}
{- |
Module: Prototype.Backend.LiveState
Description: Live state server
-}
module Prototype.Backend.LiveState
  () where

import "start-servant" Prototype.Runtime.Storage


-- | The output medium for the live state. 
data LiveStateOutput = Repl
  deriving Show

{- |

= Synopsis:

The live state of the backend offers a way to visualise and alter the "state" of the application. By "state" here, we refer to the
storage state. This lets the /executor/ of the service, modify & visualise the stored values on the fly, irrespective of the UI.

This greatly aids in the process of rapid prototyping where the executing party doesn't need to fiddle around with the UI. 

= Note:

In the future, we'd also like to support additional modes like a @Brick@ etc., if needed. 
-}

class LiveState storage (output :: LiveStateOutput) where


