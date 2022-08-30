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
  , inputText
  , submitButton
  ) where

import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!)
                                                , Html
                                                )
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


--------------------------------------------------------------------------------
inputText
  :: Text -> H.AttributeValue -> Maybe H.AttributeValue -> Maybe Text -> Html
inputText label name mvalue mhelp = H.div ! A.class_ "o-form-group" $ do
  H.label ! A.class_ "o-form-group__label" ! A.for name $ H.toHtml label
  H.div
    ! A.class_ "o-form-group__controls o-form-group__controls--full-width"
    $ do
        maybe identity (\value -> (! (A.value value))) mvalue
          $ H.input
          ! A.class_ "c-input"
          ! A.id name
          ! A.name name
        maybe mempty ((H.p ! A.class_ "c-form-help-text") . H.text) mhelp

submitButton :: H.AttributeValue -> Html -> Html
submitButton submitUrl label =
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
    $ label
