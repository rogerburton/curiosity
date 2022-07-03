
{- |
Module: Prototype.Exe.Repl.Parse
Description: Parser helpers.

Note to self:

@


-- λ> P.parse userIdParser "" "'far         '"
-- Left (ParseErrorBundle {bundleErrors = FancyError 13 (fromList [ErrorCustom (ParseErrBundle (ParseErrorBundle {bundleErrors = TrivialError 3 (Just (Tokens (' ' :| ""))) (fromList [Label ('a' :| "lphanumeric character"),EndOfInput]) :| [], bundlePosState = PosState {pstateInput = "far         ", pstateOffset = 0, pstateSourcePos = SourcePos {sourceName = "far         ", sourceLine = Pos 1, sourceColumn = Pos 1}, pstateTabWidth = Pos 8, pstateLinePrefix = ""}}))]) :| [], bundlePosState = PosState {pstateInput = "'far         '", pstateOffset = 0, pstateSourcePos = SourcePos {sourceName = "", sourceLine = Pos 1, sourceColumn = Pos 1}, pstateTabWidth = Pos 8, pstateLinePrefix = ""}})
-- *Prototype.Exe.Data Prototype.Exe.Data Commence.InteractiveState.Class P Prototype.Exe.Data.User Data.Char Char MP
-- λ> either putStrLn (putStrLn . show @UserId @Text) it

-- <interactive>:88:8: error:
--     • Could not deduce (Print (P.ParseErrorBundle Text P.ParseErr))
--         arising from a use of ‘putStrLn’
--       from the context: MonadIO m
--         bound by the inferred type of it :: MonadIO m => m ()
--         at <interactive>:88:1-50
--     • In the first argument of ‘either’, namely ‘putStrLn’
--       In the expression:
--         either putStrLn (putStrLn . show @UserId @Text) it
--       In an equation for ‘it’:
--           it = either putStrLn (putStrLn . show @UserId @Text) it
-- *Prototype.Exe.Data Prototype.Exe.Data Commence.InteractiveState.Class P Prototype.Exe.Data.User Data.Char Char MP
-- λ> either (putStrLn . MP.errorBundlePretty )  (putStrLn . show @UserId @Text) it
-- 1:14:
--   |
-- 1 | 'far         '
--   |              ^
-- far         :1:4:
--   |
-- 1 | far
--   |    ^
-- unexpected space
-- expecting alphanumeric character or end of input

@
-}
module Prototype.Exe.Repl.Parse
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
  , str
  , withTrailSpaces
  , punctuated
  , manyText
  , alphaNumText
  , asciiText
  , parseListOf
  ) where

import qualified Data.Set                      as Set
import qualified Data.Text                     as T
import           GHC.Base                       ( String )
import qualified GHC.Show
import           Network.HTTP.Types.Status      ( unprocessableEntity422 )
import qualified Commence.Runtime.Errors      as Errs
import           Text.Megaparsec               as MP
import           Text.Megaparsec.Char          as MP.Char

data CustomErrInfo = CustomErrInfo
  deriving (Eq, Ord)

data ParseErr = ParseErr Text
              | ParseErrBundle (MP.ParseErrorBundle Text ParseErr)
              deriving Eq

instance Show ParseErr where
  show = MP.showErrorComponent

-- | A rather arbitrary Ord instance, needed just to satisfy the head of `MP.ShowErrorComponent`, which is pretty poorly designed.
instance Ord ParseErr where
  ParseErr{}       <= _                = True -- ParseErr is smaller than everything else 
  _                <= ParseErr{}       = False -- ParseErr is smaller than everything else
  ParseErrBundle{} <= ParseErrBundle{} = True -- consider these equal

-- | Needs Ord.
instance MP.ShowErrorComponent ParseErr where
  showErrorComponent = \case
    ParseErr       msg    -> T.unpack msg
    ParseErrBundle bundle -> MP.errorBundlePretty bundle

-- | FIXME: The current implementation is pretty rudimentary, more informative errors to be added pretty soon.
instance Errs.IsRuntimeErr ParseErr where
  errCode _ = "ERR.INVALID_PARSE"
  httpStatus _ = unprocessableEntity422
  userMessage = Just . T.pack . MP.showErrorComponent
  displayErr  = T.pack . MP.showErrorComponent

type ParserText = MP.Parsec ParseErr Text

parseInputCtx :: ParserText a -> Text -> Either ParseErr a
parseInputCtx parser text =
  first ParseErrBundle $ MP.parse parser (T.unpack text) text

str :: MP.Tokens Text -> ParserText Text
str = MP.Char.string'

-- | Expect a string with at least 1 whitespace character. 
withTrailSpaces :: MP.Tokens Text -> ParserText Text
withTrailSpaces txt = MP.Char.string' txt <* MP.Char.space1

tryAlts :: Foldable f => f (ParserText a) -> ParserText a
tryAlts parsers = case toList parsers of
  (h : rest) -> MP.try h <|> tryAlts rest
  []         -> MP.fancyFailure noParsers
  where noParsers = Set.singleton $ MP.ErrorFail "No parsers!"

punctuated :: ParserText a -> ParserText a
punctuated p = do
  -- start with a punctuation character. 
  punc               <- MP.Char.punctuationChar
  -- Now, extract the text within the punctuations. 
  withinPunctuations <- MP.takeWhileP (Just "WithinPunctuations") (/= punc)
  -- Next, we run the provided parser on this text as input.
  res                <-
    case
      MP.parse (p <* MP.eof) (T.unpack withinPunctuations) withinPunctuations
    of
      Left err ->
        MP.fancyFailure . Set.singleton . MP.ErrorCustom . ParseErrBundle $ err
      Right res -> pure res
  -- Now, we should consume the last punctuation.
  MP.Char.char punc
  pure res
-- | Alpha numeric text.
alphaNumText :: ParserText Text
alphaNumText = manyText $ MP.many MP.Char.alphaNumChar

asciiText :: ParserText Text
asciiText = manyText $ MP.many MP.Char.asciiChar

-- | Pack a parser's output where the output is `String` as `Text`
manyText :: ParserText String -> ParserText Text
manyText = fmap T.pack

-- | Parse a list of elements using a given parser. The list needs to be in the usual list syntax.:w
parseListOf :: ParserText a -> ParserText [a]
parseListOf p =
  (MP.Char.string' "[" *> MP.Char.space)
    *> (p `MP.sepBy` MP.try comma)
    <* (MP.Char.space *> MP.Char.string' "]")
  where comma = MP.Char.space *> MP.Char.string' "," *> MP.Char.space
