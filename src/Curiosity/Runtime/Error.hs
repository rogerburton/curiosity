module Curiosity.Runtime.Error
  ( IOErr(..)
  , UnspecifiedErr(..)
  ) where

import qualified Commence.Runtime.Errors       as Errs
import qualified Data.Text                     as T
import qualified Network.HTTP.Types            as HTTP

--------------------------------------------------------------------------------
-- TODO Integrity check:
-- All UserIds must resolve: _entityUsersAndRoles.
-- List containing UserIds should not have duplicates: _entityUsersAndRoles.


--------------------------------------------------------------------------------
newtype IOErr = FileDoesntExistErr FilePath
  deriving Show

instance Errs.IsRuntimeErr IOErr where
  errCode FileDoesntExistErr{} = "ERR.FILE_NOT_FOUND"
  httpStatus FileDoesntExistErr{} = HTTP.notFound404
  userMessage = Just . \case
    FileDoesntExistErr fpath -> T.unwords ["File doesn't exist:", T.pack fpath]

-- | A placeholder error type, used until better handling (at the call site) is
-- put in place.
newtype UnspecifiedErr = UnspecifiedErr Text
  deriving Show

instance Errs.IsRuntimeErr UnspecifiedErr where
  errCode UnspecifiedErr{} = "ERR.FILE_NOT_FOUND"
  httpStatus UnspecifiedErr{} = HTTP.notFound404
  userMessage = Just . \case
    UnspecifiedErr msg -> T.unwords ["Error:", msg]
