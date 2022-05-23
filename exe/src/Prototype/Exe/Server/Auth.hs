{-# LANGUAGE
    TypeOperators
  , DataKinds
  , TypeFamilies
#-}
{- |
Module: Prototype.Server.New.Auth
Description: Authentication and authorization module.

-}

module Prototype.Exe.Server.Auth
  ( UserAuthentication
  , PostAuthHeaders
  , Context
  ) where

import qualified Prototype.Exe.Data.User   as User
import           Servant.API
import qualified Servant.Auth.Server           as SAuth
import qualified Servant.Server                as Server

-- brittany-disable-next-binding
-- | Simple user authentication.
type UserAuthentication = SAuth.Auth '[SAuth.Cookie] User.UserId

-- | Headers that will be returned post a successful authentication.
type PostAuthHeaders
  = '[ Header "Location" Text
     , Header "Set-Cookie" SAuth.SetCookie
     , Header "Set-Cookie" SAuth.SetCookie
     ]

type Context = Server.Context

-- {- | The data that is the outcome of successful authorization over a particular resource.

-- As a result, the resource itself should be returned; along with the grant the accessor holds over this resource.
-- -}
-- type instance AuthServerData (AuthProtect (ResourceAuth res tr))
--   = ResourceAuth res tr
