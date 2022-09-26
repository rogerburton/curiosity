{- |
Module: Curiosity.Html.Run
Description: A page to run a `cty run` command.
-}
module Curiosity.Html.Run
  ( RunPage(..)
  ) where

import qualified Curiosity.Data.User           as User
import           Curiosity.Html.Misc
import           Curiosity.Html.Navbar          ( navbar )
import qualified Smart.Html.Dsl                as Dsl
import           Smart.Html.Layout
import qualified Smart.Html.Misc               as Misc
import qualified Smart.Html.Render             as Render
import           Smart.Html.Shared.Html.Icons   ( svgIconAdd
                                                , svgIconArrowRight
                                                )
import           Smart.Html.SideMenu            ( SideMenu(..)
                                                , SideMenuItem(..)
                                                )
import           Text.Blaze                     ( customAttribute )
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A


--------------------------------------------------------------------------------
data RunPage = RunPage
  { _runPageUserProfile :: Maybe User.UserProfile
  , _runPageSubmitURL   :: H.AttributeValue
  }

instance H.ToMarkup RunPage where
  toMarkup (RunPage mprofile submitUrl) =
    renderForm' mprofile $ groupLayout $ do
      title "Run a command"
      inputText
          "Command"
          "command"
          Nothing
        $ Just
            "A command similar to what `cty run` can accept."
      submitButton submitUrl "Run"
