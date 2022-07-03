{-# LANGUAGE
    DataKinds
#-}
module Prototype.Exe.Server.Private.Auth
  ( UserAuthentication
  , PostAuthHeaders
  ) where

import           Commence.Server.Auth           ( PostAuthHeaders )
import qualified Prototype.Exe.Data.User       as User
import qualified Servant.Auth.Server           as SAuth

-- brittany-disable-next-binding
-- | Simple user authentication.
type UserAuthentication = SAuth.Auth '[SAuth.Cookie] User.UserId
