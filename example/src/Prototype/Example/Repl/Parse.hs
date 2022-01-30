module Prototype.Example.Repl.Parse
  ( ParseErr(..)
  , ParserText
  -- * Re-exports
  -- ** All of @Megaparsec@ for convenience, and re-exports in this case
  -- shouldn't be misleading. 
  , module MP
  , module MP.Char
  -- * Running parsers
  , parseInputCtx
  , tryAlts
  -- * Common parser combinators
  , withTrailSpaces
  ) where

import qualified Data.Set                      as Set
import qualified Data.Text                     as T
import qualified Prototype.Runtime.Errors      as Errs
import           Text.Megaparsec               as MP
import           Text.Megaparsec.Char          as MP.Char

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

-- | Expect a string with at least 1 whitespace character. 
withTrailSpaces :: MP.Tokens Text -> ParserText ()
withTrailSpaces txt = MP.Char.string' txt *> MP.Char.space1

tryAlts :: Foldable f => f (ParserText a) -> ParserText a
tryAlts = foldl' untilSuccess $ MP.fancyFailure noParsers
 where
  untilSuccess tried parser = tried <|> MP.try parser
  noParsers = Set.singleton $ MP.ErrorFail "No parsers!"
