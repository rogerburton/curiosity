{-# LANGUAGE TupleSections #-}
module Curiosity.Interpret
  ( interpretFile
  , interpret'
  , formatOutput
  , wordsq
  ) where

import qualified Curiosity.Command             as Command
import qualified Curiosity.Data.User           as User
import qualified Curiosity.Runtime             as Rt
import           Data.List                      ( last )
import qualified Data.Text                     as T
import qualified Options.Applicative           as A
import           System.FilePath                ( (</>)
                                                , takeDirectory
                                                )


--------------------------------------------------------------------------------
interpretFile
  :: Rt.Runtime
  -> User.UserName
  -> FilePath
  -> Int
  -> IO [(Int, ExitCode, Text)]
interpretFile runtime user path nesting = do
  let dir = takeDirectory path
  content <- T.lines <$> readFile path
  interpret' runtime user dir content nesting

interpret'
  :: Rt.Runtime
  -> User.UserName
  -> FilePath
  -> [Text]
  -> Int
  -> IO [(Int, ExitCode, Text)]
interpret' runtime user dir content nesting = do
  (_, output) <- foldlM loop (user, []) $ zip [1 :: Int ..] content
  pure output
 where
  loop (user', acc) (ln, line) = do
    let (prefix, _) = T.breakOn "#" line
        separated   = map T.pack . wordsq $ T.unpack prefix
        grouped     = T.unwords separated
    case separated of
      []               -> pure (user', acc)
      ["as", username] -> do
        let output = [show ln <> ": " <> grouped, "Modifying default user."]
        pure
          (User.UserName username, acc ++ map (nesting, ExitSuccess, ) output)
      ["quit"] -> do
        let output = [show ln <> ": " <> grouped, "Exiting."]
        pure (user', acc ++ map (nesting, ExitSuccess, ) output)
      input -> do
        let output_ = [show ln <> ": " <> grouped]
            result =
              A.execParserPure A.defaultPrefs Command.parserInfo
                $   T.unpack
                <$> input
        case result of
          A.Success command -> do
            case command of
              Command.Reset _ -> do
                let output = output_ ++ ["Resetting to the empty state."]
                Rt.reset runtime
                pure (user', acc ++ map (nesting, ExitSuccess, ) output)
              Command.Run _ scriptPath -> do
                output' <- liftIO $ interpretFile runtime
                                                       user
                                                       (dir </> scriptPath)
                                                       (succ nesting)
                pure
                  ( user'
                  , acc ++ map (nesting, ExitSuccess, ) output_ ++ output'
                  )
              _ -> do
                (_, output) <- Rt.handleCommand runtime user' command
                pure
                  ( user'
                  , acc ++ map (nesting, ExitSuccess, ) (output_ ++ output)
                  )
          A.Failure err ->
            pure (user', acc ++ [(nesting, ExitFailure 1, show err)])
          A.CompletionInvoked _ ->
            pure (user', acc ++ [(nesting, ExitFailure 1, "Shouldn't happen")])


--------------------------------------------------------------------------------
formatOutput :: [(Int, ExitCode, Text)] -> (ExitCode, [Text])
formatOutput output =
  let len     = length $ takeWhile ((== ExitSuccess) . snd3) output
      output' = take (len + 1) output
      pad 0 = ""
      pad 1 = "> "
      pad n = T.concat (replicate ((n - 1) * 2) ">") <> "> "
      ls =  map (\(a, _, c) -> pad a <> c) output'
  in (snd3 . last $ (0, ExitSuccess, "") : output', ls)

snd3 (_, b, _) = b


--------------------------------------------------------------------------------
-- From https://stackoverflow.com/questions/4334897/functionally-split-a-string-by-whitespace-group-by-quotes
wordsq = outside [] . (' ' :)

add c res = if null res then [[c]] else map (++ [c]) res

outside res xs = case xs of
  ' ' : ' '  : ys -> outside res $ ' ' : ys
  ' ' : '\"' : ys -> res ++ inside [] ys
  ' '        : ys -> res ++ outside [] ys
  c          : ys -> outside (add c res) ys
  _               -> res

inside res xs = case xs of
  ' '  : ' ' : ys -> inside res $ ' ' : ys
  '\"' : ' ' : ys -> res ++ outside [] (' ' : ys)
  '\"'       : [] -> res
  c          : ys -> inside (add c res) ys
  _               -> res

