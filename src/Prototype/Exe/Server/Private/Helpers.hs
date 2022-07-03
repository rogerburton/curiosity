{-# LANGUAGE DataKinds #-}
module Prototype.Exe.Server.Private.Helpers
  ( GetUserPage
  , PutUserPage
  , DeleteUserPage
  , PostUserPage
  , UserPage
  ) where

import qualified Prototype.Exe.Data.User       as User
import           Servant.API
import qualified Servant.HTML.Blaze            as B
import qualified "design-hs-lib" Smart.Server.Page
                                               as SS.P

type GetUserPage pageData = UserPage Get pageData
type PutUserPage pageData = UserPage Put pageData
type DeleteUserPage pageData = UserPage Delete pageData
type PostUserPage pageData = UserPage Post pageData

-- | A convenient alias to denote a GET endpoint to get a user-authenticated page.
type UserPage method pageData
  = method '[B.HTML] (SS.P.Page 'SS.P.Authd User.UserProfile pageData)
