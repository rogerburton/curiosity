{-# LANGUAGE
    TypeOperators
  , DataKinds
  , TypeFamilies
#-}
{- |
Module: Prototype.Server.New.Auth
Description: Authentication and authorization module.

-}

module Prototype.Example.Server.Auth
  ( UserAuthentication
  , PostAuthHeaders
  ) where

import           Prototype.Types
import           Servant.API
import qualified Servant.Auth.Server           as SAuth

-- brittany-disable-next-binding
-- | Simple user authentication.
type UserAuthentication = SAuth.Auth '[SAuth.Cookie] User

-- | Headers that will be returned post a successful authentication.
type PostAuthHeaders
  = '[ Header "Location" Text
     , Header "Set-Cookie" SAuth.SetCookie
     , Header "Set-Cookie" SAuth.SetCookie
     ]

-- {- | The data that is the outcome of successful authorization over a particular resource.

-- As a result, the resource itself should be returned; along with the grant the accessor holds over this resource.
-- -}
-- type instance AuthServerData (AuthProtect (ResourceAuth res tr))
--   = ResourceAuth res tr
