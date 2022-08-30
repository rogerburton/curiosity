{- |
Module: Curiosity.Html.Employment
Description: Employment contract pages (view and edit).
-}
module Curiosity.Html.Employment
  ( ContractView(..)
  ) where

import qualified Curiosity.Data.Employment     as Employment
import           Curiosity.Html.Misc
import           Curiosity.Html.Navbar          ( navbar )
import qualified Smart.Html.Dsl                as Dsl
import qualified Smart.Html.Render             as Render
import           Smart.Html.Shared.Html.Icons   ( svgIconEdit )
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A


--------------------------------------------------------------------------------
data ContractView = ContractView
  { _contractViewContract      :: Employment.Contract
  , _contractViewHasEditButton :: Bool
  }

instance H.ToMarkup ContractView where
  toMarkup (ContractView contract hasEditButton) =
    Render.renderCanvasFullScroll
      . Dsl.SingletonCanvas
      $ H.div
      ! A.class_ "c-app-layout u-scroll-vertical"
      $ do
          H.header $ H.toMarkup . navbar $ "TODO username"
          fullScroll $ contractView contract hasEditButton

contractView contract hasEditButton =
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
            $ "Employment contract"
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
          keyValuePair "ID" (Employment._contractId contract)
