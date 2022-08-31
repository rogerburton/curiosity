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
    renderView $ contractView contract hasEditButton

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
          when hasEditButton $ editButton "#"
    H.dl
      ! A.class_ "c-key-value c-key-value--horizontal c-key-value--short"
      $ do
          keyValuePair "ID" (Employment._contractId contract)
