module Prototype.Example.Server.Shared.Html.Helpers.Form
  ( mkButton
  ) where

import           Network.HTTP.Types.Method      ( StdMethod
                                                , renderStdMethod
                                                )
import qualified "design-hs-lib" Smart.Html.Button
                                               as Btn
import qualified "design-hs-lib" Smart.Html.Shared.Types
                                               as HTypes
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import           Text.Blaze.Html5.Attributes

mkButton :: HTypes.Title -> H.AttributeValue -> StdMethod -> H.Markup
mkButton text submitUrl method' =
  H.toMarkup (Btn.ButtonPrimary text HTypes.Enabled)
    ! formaction submitUrl
    ! formmethod (H.textValue . decodeUtf8 $ renderStdMethod method')

