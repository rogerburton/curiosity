module Curiosity.Runtime.Error
  ( IOErr(..)
  , UnspeciedErr(..)
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
newtype UnspeciedErr = UnspeciedErr Text
  deriving Show

instance Errs.IsRuntimeErr UnspeciedErr where
  errCode UnspeciedErr{} = "ERR.FILE_NOT_FOUND"
  httpStatus UnspeciedErr{} = HTTP.notFound404
  userMessage = Just . \case
    UnspeciedErr msg -> T.unwords ["Error:", msg]
