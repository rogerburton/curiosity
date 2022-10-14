{- |
Module: Curiosity.Html.Homepage
Description: The homepage for Curiosity (when a user is logged in).
-}
module Curiosity.Html.Homepage
  ( WelcomePage(..)
  ) where

import qualified Curiosity.Data.Email          as Email
import qualified Curiosity.Data.Quotation      as Quotation
import qualified Curiosity.Data.User           as User
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
  , welcomePageQuotationForms :: [(Text, Quotation.CreateQuotationAll)]
    -- ^ Email addr. needing verif., if the user has the right to perform the
    -- corresponding action.
  , welcomePageEmails         :: [Email.Email]
    -- ^ Enqueued emails being sent to the logged user.
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
            maybe (pure ()) panelEmailAddrToVerify welcomePageEmailAddrToVerify

            panelQuotationForms welcomePageQuotationForms

            panelSentEmails welcomePageEmails

panelEmailAddrToVerify :: [User.UserProfile] -> H.Html
panelEmailAddrToVerify profiles =
  panel "Email addresses to verify" $ Misc.table titles display profiles
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

panelQuotationForms :: [(Text, Quotation.CreateQuotationAll)] -> H.Html
panelQuotationForms forms =
  panel "Quotation forms" $ Misc.table titles display forms
 where
  titles = ["Key", "Name"]
  display (key, Quotation.CreateQuotationAll {}) =
    ( [key, "TODO"]
    , []
    , (Just $ "/edit/quotation/" <> key)
    )

-- | Display enqueued emails that are to the logged in user.
panelSentEmails :: [Email.Email] -> H.Html
panelSentEmails emails =
  panel "Emails to be received" $ Misc.table titles display emails
 where
  titles = ["ID", "Template"]
  display Email.Email {..} =
    ( [Email.unEmailId _emailId, Email.emailTemplateName _emailTemplate]
    , []
    , Nothing
    )

panel title body =
  H.div
    ! A.class_ "o-container o-container--large"
    $ H.div
    ! A.class_ "o-container-vertical"
    $ H.div
    ! A.class_ "u-padding-vertical-s"
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
          $ H.text title
        H.div ! A.class_ "c-panel__body" $ body
