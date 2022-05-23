{-# LANGUAGE DataKinds #-}
module Prototype.Exe.Server.Private.Helpers
  ( GetUserPage
  , PutUserPage
  , DeleteUserPage
  , PostUserPage
  , UserPage
  ) where

import qualified Prototype.Exe.Data.User   as User
import qualified "start-servant" Prototype.Server.New.Page
                                               as SS.P
import           Servant.API
import qualified Servant.HTML.Blaze            as B

type GetUserPage pageData = UserPage Get pageData
type PutUserPage pageData = UserPage Put pageData
type DeleteUserPage pageData = UserPage Delete pageData
type PostUserPage pageData = UserPage Post pageData

-- | A convenient alias to denote a GET endpoint to get a user-authenticated page.
type UserPage method pageData
  = method '[B.HTML] (SS.P.Page 'SS.P.Authd User.UserProfile pageData)
