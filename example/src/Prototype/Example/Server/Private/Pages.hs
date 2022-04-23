{-# LANGUAGE DataKinds #-}
{- |
Module: Prototype.Example.Server.Private.Pages
Description: Private pages for the application

The goal is to exemplify the use of our DSL for some simple pages and to have something more tangible to show.

-}
module Prototype.Example.Server.Private.Pages
  ( WelcomePage(..)
  ) where

import qualified Text.Blaze.Html5              as H

-- | A simple welcome page. 
data WelcomePage = WelcomePage

instance H.ToMarkup WelcomePage where
  toMarkup _ = "Welcome to the welcome page!"

