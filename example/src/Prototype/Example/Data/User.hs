{- |
Module: Prototype.Example.Data.User
Description: User related datatypes
-}
module Prototype.Example.Data.User
  ( UserProfile
  , UserId(..)
  ) where

data UserProfile

newtype UserId = UserId Text
               deriving (Eq, Show)
               deriving IsString via Text
