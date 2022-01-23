module Prototype.Example.Repl.Parse
  ( ParseErr(..)
  , ParserText
  -- * Re-exports

  -- ** All of @Megaparsec@ for convenience, and re-exports in this case
  -- shouldn't be misleading. 
  , module MP
  -- * Running parsers
  , parseInputCtx
  ) where

import qualified Data.Text                     as T
import qualified Prototype.Runtime.Errors      as Errs
import           Text.Megaparsec               as MP

data CustomErrInfo = CustomErrInfo
  deriving (Eq, Ord)

newtype ParseErr = ParseErr Text
                 deriving (Show, Eq, Ord)

-- | FIXME: The current implementation is pretty rudimentary, more informative errors to be added pretty soon.
instance Errs.IsRuntimeErr ParseErr where
  errCode _ = "ERR.INVALID_PARSE"
  httpStatus _ = undefined
  userMessage (ParseErr mpErr) = Just $ show mpErr

type ParserText = MP.Parsec ParseErr Text

parseInputCtx :: ParserText a -> Text -> Either ParseErr a
parseInputCtx parser text =
  first (ParseErr . show) $ MP.parse parser (T.unpack text) text
