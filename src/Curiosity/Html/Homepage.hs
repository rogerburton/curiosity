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
import qualified Smart.Html.Misc               as Misc
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
    Render.renderCanvasFullScroll
      . Dsl.SingletonCanvas
      $ H.div
      ! A.class_ "c-app-layout u-scroll-vertical"
      $ do
          H.header
            $ H.toMarkup
            . navbar
            . User.unUserName
            . User._userCredsName
            $ User._userProfileCreds welcomePageUser
          H.main ! A.class_ "u-scroll-wrapper" $ do
            case welcomePageEmailAddrToVerify of
              Just profiles -> do
                H.div
                  ! A.class_ "o-container o-container--large"
                  $ H.div
                  ! A.class_ "o-container-vertical"
                  $ H.div
                  ! A.class_ "u-padding-vertical-l"
                  $ H.div
                  ! A.class_ "c-panel"
                  $ do
                      H.div
                        ! A.class_ "c-panel__header"
                        $ H.div
                        ! A.class_ "c-toolbar"
                        $ H.div
                        ! A.class_ "c-toolbar__left"
                        $ H.h2
                        ! A.class_ "c-panel__title"
                        $ "Email addresses to verify"
                      H.div ! A.class_ "c-panel__body" $ Misc.table titles
                                                                    display
                                                                    profiles
              Nothing -> pure ()
   where
    titles = ["ID", "Username", "Email addr."]
    display User.UserProfile {..} =
      let User.UserId        i = _userProfileId
          User.UserName      n = User._userCredsName _userProfileCreds
          User.UserEmailAddr e = _userProfileEmailAddr
      in  ( [i, n, e]
          , [ ( Misc.divIconCheck
              , "Set as verified"
              , "/action/set-email-addr-as-verified/" <> n
              )
            ]
          , (Just $ "/" <> n)
          )
