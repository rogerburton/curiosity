{- |
Module: Curiosity.Html.Homepage
Description: The homepage for Curiosity (when a user is logged in).
-}
module Curiosity.Html.Homepage
  ( WelcomePage(..)
  ) where

import           Curiosity.Data.User           as User
import           Curiosity.Html.Navbar          ( navbar )
import qualified Smart.Html.Dsl                as Dsl
import qualified Smart.Html.Render             as Render
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A


--------------------------------------------------------------------------------
-- | A simple welcome page.
data WelcomePage = WelcomePage
  { welcomePageUser              :: User.UserProfile
    -- ^ The logged in user.
  , welcomePageEmailAddrToVerify :: Maybe [User.UserProfile]
    -- ^ Email addr. needing verif., if the user has the right to perform the
    -- corresponding action.
  }

instance H.ToMarkup WelcomePage where
  toMarkup WelcomePage {..} =
    Render.renderCanvas
      . Dsl.SingletonCanvas
      $ H.div
      ! A.class_ "c-app-layout"
      $ do
          H.header
            $ H.toMarkup
            . navbar
            . User.unUserName
            . User._userCredsName
            $ User._userProfileCreds welcomePageUser
          H.body $ do
            case welcomePageEmailAddrToVerify of
              Just profiles -> do
                let f User.UserProfile {..} =
                      let User.UserId i = _userProfileId
                          User.UserName n =
                            User._userCredsName _userProfileCreds
                      in  H.li . H.toHtml $ i <> " " <> n
                H.h1 "Email addresses to verify"
                H.ul $ mapM_ f profiles
              Nothing -> pure ()
