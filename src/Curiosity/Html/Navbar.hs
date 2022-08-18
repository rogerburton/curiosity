{- |
Module: Curiosity.Html.Navbar
Description: A navigation bar for Curiosity.
-}
module Curiosity.Html.Navbar
  ( navbar
  ) where

import           Smart.Html.Avatar
import qualified Smart.Html.Navbar             as Navbar


--------------------------------------------------------------------------------
navbar :: Text -> Navbar.Navbar
navbar name = Navbar.Navbar [] [userEntry name]

userEntry :: Text -> Navbar.RightEntry
userEntry name = Navbar.UserEntry (userEntries name) NoAvatarImage

userEntries :: Text -> [Navbar.SubEntry]
userEntries name =
  [ Navbar.SignedInAs name
  , Navbar.Divider
  , Navbar.SubEntry "Settings" "/settings/profile" False
  , Navbar.Divider
  -- TODO: change to `POST` in the future. 
  , Navbar.SubEntry "Sign out" "/a/logout" False
  ]
