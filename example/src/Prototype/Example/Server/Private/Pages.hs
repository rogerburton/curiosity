{-# LANGUAGE DataKinds #-}
{- |
Module: Prototype.Example.Server.Private.Pages
Description: Private pages for the application

The goal is to exemplify the use of our DSL for some simple pages and to have something more tangible to show.

-}
module Prototype.Example.Server.Private.Pages
  ( WelcomePage(..)
  ) where

import           Control.Lens
import qualified Prototype.Example.Data.User   as User
import qualified "start-servant" Prototype.Server.New.Page
                                               as SS.P
import qualified "design-hs-lib" Smart.Html.Button
                                               as Btn
import qualified "design-hs-lib" Smart.Html.Dsl
                                               as Dsl
import qualified "design-hs-lib" Smart.Html.Form
                                               as Form
import qualified "design-hs-lib" Smart.Html.Input
                                               as Inp
import qualified "design-hs-lib" Smart.Html.Shared.Types
                                               as HTypes
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import           Text.Blaze.Html5.Attributes

-- | A simple welcome page. 
data WelcomePage = WelcomePage

instance H.ToMarkup WelcomePage where
  toMarkup = undefined

-- FIXME: orphan instance.
-- Ideally, we'd like to render the navbar using the user-data. For this, we'd like a new typeclass in start-servant to render the navbar. 
instance H.ToMarkup page => H.ToMarkup (SS.P.Page 'SS.P.Authd User.UserProfile page) where
