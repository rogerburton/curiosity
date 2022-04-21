{-# LANGUAGE
    DataKinds
#-}
module Prototype.Example.Server.Private.Auth
  ( UserAuthentication
  , PostAuthHeaders
  ) where

import qualified Prototype.Example.Data.User   as User
import           Prototype.Server.New.Auth      ( PostAuthHeaders )
import qualified Servant.Auth.Server           as SAuth

-- brittany-disable-next-binding
-- | Simple user authentication.
type UserAuthentication = SAuth.Auth '[SAuth.Cookie] User.UserProfile
