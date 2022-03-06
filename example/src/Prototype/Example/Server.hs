{-# LANGUAGE DataKinds, TypeOperators #-} -- Language extensions needed for servant. 
{- |
Module: Prototype.Example.Server
Description: Server root module, split up into public and private sub-modules.

-}
module Prototype.Example.Server
  ( Example
  , exampleT
  ) where

import           Prototype.Example.Server.Public
                                               as Pub
import           Servant

type Example = "public" :> Pub.Public

exampleT :: forall m . ServerT Pub.Public m
exampleT = Pub.publicT
