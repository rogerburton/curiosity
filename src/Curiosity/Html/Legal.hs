{- |
Module: Curiosity.Html.Legal
Description: Legal entity pages (view and edit).
-}
module Curiosity.Html.Legal
  ( EntityView(..)
  , CreateEntityPage(..)
  ) where

import qualified Curiosity.Data.Legal          as Legal
import qualified Curiosity.Data.User           as User
import           Curiosity.Html.Misc
import           Curiosity.Html.Navbar          ( navbar )
import qualified Smart.Html.Dsl                as Dsl
import qualified Smart.Html.Render             as Render
import           Smart.Html.Shared.Html.Icons   ( svgIconEdit )
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A


--------------------------------------------------------------------------------
data EntityView = EntityView
  { _entityViewEntity        :: Legal.Entity
  , _entityViewHasEditButton :: Bool
  }

instance H.ToMarkup EntityView where
  toMarkup (EntityView entity hasEditButton) =
    Render.renderCanvasFullScroll
      . Dsl.SingletonCanvas
      $ H.div
      ! A.class_ "c-app-layout u-scroll-vertical"
      $ do
          H.header $ H.toMarkup . navbar $ "TODO username"
          fullScroll $ entityView entity hasEditButton

entityView entity hasEditButton =
  containerLarge $ H.div ! A.class_ "u-spacer-bottom-xl" $ do
    H.div
      ! A.class_ "u-spacer-bottom-l"
      $ H.div
      ! A.class_ "c-navbar c-navbar--unpadded c-navbar--bordered-bottom"
      $ H.div
      ! A.class_ "c-toolbar"
      $ do
          H.div
            ! A.class_ "c-toolbar__left"
            $ H.h3
            ! A.class_ "c-h3 u-m-b-0"
            $ "Legal entity"
          when hasEditButton
            $ H.div
            ! A.class_ "c-toolbar__right"
            $ H.a
            ! A.class_ "c-button c-button--secondary"
            ! A.href "/settings/profile/edit"
            $ H.span
            ! A.class_ "c-button__content"
            $ do
                H.div ! A.class_ "o-svg-icon o-svg-icon-edit" $ H.toHtml
                  svgIconEdit
                H.span ! A.class_ "c-button__label" $ "Edit"
    H.dl
      ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short"
      $ do
          keyValuePair "ID"                (Legal._entityId entity)
          keyValuePair "Registration name" (Legal._entityName entity)
          keyValuePair "CBE number"        (Legal._entityCbeNumber entity)
          keyValuePair "VAT number"        (Legal._entityVatNumber entity)


--------------------------------------------------------------------------------
data CreateEntityPage = CreateEntityPage
  { _createEntityPageUserProfile :: User.UserProfile
    -- ^ The user creating the entity
  , _createEntityPageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup CreateEntityPage where
  toMarkup (CreateEntityPage profile submitUrl) =
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
            $ User._userProfileCreds profile
          H.main ! A.class_ "u-maximize-width" $ entityCreationForm profile
                                                                    submitUrl

entityCreationForm profile submitUrl = containerMedium $ do
  H.div
    ! A.class_ "u-spacer-bottom-l"
    $ H.div
    ! A.class_ "c-navbar c-navbar--unpadded c-navbar--bordered-bottom"
    $ H.div
    ! A.class_ "c-toolbar"
    $ H.div
    ! A.class_ "c-toolbar__left"
    $ H.h3
    ! A.class_ "c-h3 u-m-b-0"
    $ "New legal entity"
  H.div
    ! A.class_ "o-form-group-layout o-form-group-layout--horizontal"
    $ H.form
    $ do
        H.div ! A.class_ "o-form-group" $ do
          H.label
            ! A.class_ "o-form-group__label"
            ! A.for "name"
            $ "Registration name"
          H.div
            ! A.class_
                "o-form-group__controls o-form-group__controls--full-width"
            $ H.input
            ! A.class_ "c-input"
            ! A.id "name"
            ! A.name "name"
        H.div ! A.class_ "o-form-group" $ do
          H.label
            ! A.class_ "o-form-group__label"
            ! A.for "cbe-number"
            $ "CBE number"
          H.div
            ! A.class_
                "o-form-group__controls o-form-group__controls--full-width"
            $ do
                H.input ! A.class_ "c-input" ! A.id "cbe-number" ! A.name
                  "cbe-number"
                H.p ! A.class_ "c-form-help-text" $ "Example: 100200300"
        H.div ! A.class_ "o-form-group" $ do
          H.label
            ! A.class_ "o-form-group__label"
            ! A.for "vat-number"
            $ "VAT number"
          H.div
            ! A.class_
                "o-form-group__controls o-form-group__controls--full-width"
            $ do
                H.input ! A.class_ "c-input" ! A.id "vat-number" ! A.name
                  "vat-number"
                H.p ! A.class_ "c-form-help-text" $ "Example: BE0100200300"
        H.div
          ! A.class_ "o-form-group"
          $ H.div
          ! A.class_ "u-spacer-left-auto"
          $ H.button
          ! A.class_ "c-button c-button--primary"
          ! A.formaction (H.toValue submitUrl)
          ! A.formmethod "POST"
          $ H.span
          ! A.class_ "c-button__content"
          $ H.span
          ! A.class_ "c-button__label"
          $ "Create new legal entity"
