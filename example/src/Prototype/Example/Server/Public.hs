{-# LANGUAGE DataKinds, TypeOperators #-}
module Prototype.Example.Server.Public
  ( Public
  , publicT
  ) where

import qualified "start-servant" Prototype.Server.New.Page
                                               as P
import           Servant
import qualified Servant.HTML.Blaze            as B

-- | A publicly available login page. 
type Public = "login" :> Get '[B.HTML] (P.Page 'P.Public P.LoginPage)

publicT :: forall m . ServerT Public m
publicT = undefined
