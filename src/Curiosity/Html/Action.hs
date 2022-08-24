{- |
Module: Curiosity.Html.Action
Description: User actions. Actions are supposed to match what is available
under e.g. `cty user do` and the action menu on table views.
-}
module Curiosity.Html.Action
  ( SetUserEmailAddrAsVerifiedPage(..)
  , ActionResult(..)
  ) where

import           Curiosity.Data.User           as User
import           Curiosity.Html.Profile         ( keyValuePair )
import qualified Smart.Html.Dsl                as Dsl
import qualified Smart.Html.Misc               as Misc
import           Smart.Html.Panel               ( Panel(..) )
import qualified Smart.Html.Render             as Render
import qualified Smart.Html.Shared.Types       as Types
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A


--------------------------------------------------------------------------------
-- | A form to perform the SetUserEmailAddrAsVerified action.
data SetUserEmailAddrAsVerifiedPage = SetUserEmailAddrAsVerifiedPage
  { setUserEmailAddrAsVerifiedForUser :: User.UserProfile
    -- ^ The user whose email address should be set as verified.
  }

instance H.ToMarkup SetUserEmailAddrAsVerifiedPage where
  toMarkup SetUserEmailAddrAsVerifiedPage {..} =
    let profile  = setUserEmailAddrAsVerifiedForUser
        username = User._userCredsName . User._userProfileCreds $ profile
    in  fullScrollWrapper . panelWrapper $ do
          H.toMarkup $ setUserEmailAddrAsVerifiedPanel username profile
          setUserEmailAddrAsVerifiedForm username

fullScrollWrapper content =
  Render.renderCanvasFullScroll
    . Dsl.SingletonCanvas
    $ H.div
    ! A.class_ "c-app-layout u-scroll-vertical"
    $ content

panelWrapper content = H.main ! A.class_ "u-scroll-wrapper" $ do
  H.div
    ! A.class_ "o-container o-container--large"
    $ H.div
    ! A.class_ "o-container-vertical"
    $ H.div
    ! A.class_ "u-padding-vertical-l"
    $ content

setUserEmailAddrAsVerifiedPanel username profile =
  PanelHeaderAndBody "Set email address as verified"
    $ H.dl
    ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short"
    $ do
        keyValuePair "Username"      username
        keyValuePair "Display name"  (User._userProfileDisplayName profile)
        keyValuePair "Email address" (User._userProfileEmailAddr profile)

setUserEmailAddrAsVerifiedForm username = H.form $ do
  H.input ! A.type_ "hidden" ! A.id "username" ! A.name "username" ! A.value
    (H.toValue username)
  H.div
    ! A.class_ "o-form-group-layout o-form-group-layout--horizontal"
    $ H.div
    ! A.class_ "o-form-group"
    $ H.div
    ! A.class_ "u-spacer-left-auto u-spacer-top-l"
    $ H.button
    ! A.class_ "c-button c-button--primary"
    ! A.formaction "/a/set-email-addr-as-verified"
    ! A.formmethod "POST"
    $ H.span
    ! A.class_ "c-button__content"
    $ do
        H.span ! A.class_ "c-button__label" $ "Set as verified"
        Misc.divIconCheck


--------------------------------------------------------------------------------
data ActionResult = ActionResult Text Text

instance H.ToMarkup ActionResult where
  toMarkup (ActionResult title msg) = fullScrollWrapper . panelWrapper $ do
    H.toMarkup $ actionResultPanel title msg

actionResultPanel title msg =
  PanelHeaderAndBody (Types.Title title) $ H.code $ H.text msg
