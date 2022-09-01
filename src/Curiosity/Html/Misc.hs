{- |
Module: Curiosity.Html.Misc
Description: Helper functions to build HTML views.

TODO Move to smart-design-hs Misc.
-}
module Curiosity.Html.Misc
  ( containerMedium
  , containerLarge
  , keyValuePair
  , fullScroll
  ) where

import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A


--------------------------------------------------------------------------------
-- | This works well paired with a side menu.
-- TODO Probably move this directly to a "layout" function such as
-- withSideMenuFullScroll.
containerMedium content =
  H.div
    ! A.class_ "u-scroll-wrapper-body"
    $ H.div
    ! A.class_ "o-container o-container--large"
    $ H.div
    ! A.class_ "o-container-vertical"
    $ H.div
    ! A.class_ "o-container o-container--medium"
    $ content

-- | This works well when not paired with a side menu.
-- TODO Probably move this directly to a "layout" function such as fullScroll.
containerLarge content =
  H.div
    ! A.class_ "u-scroll-wrapper-body"
    $ H.div
    ! A.class_ "o-container o-container--large"
    $ H.div
    ! A.class_ "o-container-vertical"
    $ content

keyValuePair :: H.ToMarkup a => Text -> a -> H.Html
keyValuePair key value = H.div ! A.class_ "c-key-value-item" $ do
  H.dt ! A.class_ "c-key-value-item__key" $ H.toHtml key
  H.dd ! A.class_ "c-key-value-item__value" $ H.toHtml value

-- | The corresponding layout with a side menu is withSideMenuFullScroll.
-- TODO Move to Smart.Html.Layout.
fullScroll content = H.main ! A.class_ "u-maximize-width" $ do
  content
