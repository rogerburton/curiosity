{- |
Module: Curiosity.Html.Action
Description: User actions. Actions are supposed to match what is available
under e.g. `cty user do` and the action menu on table views.
-}
module Curiosity.Html.Action
  ( SetUserEmailAddrAsVerifiedPage(..)
  , ActionResult(..)
  , EchoPage(..)
  ) where

import           Curiosity.Data.User           as User
import           Curiosity.Html.Misc
import qualified Smart.Html.Dsl                as Dsl
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
        keyValuePair "Username" username
        maybe mempty
              (keyValuePair "Display name")
              (User._userProfileDisplayName profile)
        keyValuePair "Email address" (User._userProfileEmailAddr profile)

setUserEmailAddrAsVerifiedForm username = H.form $ do
  H.input ! A.type_ "hidden" ! A.id "username" ! A.name "username" ! A.value
    (H.toValue username)
  button "/a/set-email-addr-as-verified" "Set as verified"


--------------------------------------------------------------------------------
data ActionResult = ActionResult Text Text

instance H.ToMarkup ActionResult where
  toMarkup (ActionResult title msg) = fullScrollWrapper . panelWrapper $ do
    H.toMarkup $ actionResultPanel title msg

actionResultPanel title msg =
  PanelHeaderAndBody (Types.Title title) $ H.code $ H.text msg


--------------------------------------------------------------------------------
data EchoPage = EchoPage Text

instance H.ToMarkup EchoPage where
  toMarkup = \case
    EchoPage msg -> withText msg
   where
    withText msg =
      Render.renderCanvas
        $ Dsl.SingletonCanvas
        $ H.div
        ! A.class_ "c-display"
        $ H.code
        $ H.toMarkup msg
