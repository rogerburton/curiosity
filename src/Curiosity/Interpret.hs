{-# LANGUAGE TupleSections #-}
module Curiosity.Interpret
  ( Trace(..)
  , handleRun
  , handleRun'
  , interpretFile
  , interpret
  , interpret'
  , formatOutput
  , flatten
  , pad
  , listScenarios
  , wordsq
  ) where

import qualified Curiosity.Command             as Command
import qualified Curiosity.Data                as Data
import qualified Curiosity.Data.User           as User
import qualified Curiosity.Parse               as P
import qualified Curiosity.Runtime             as Rt
import           Data.List                      ( last )
import qualified Data.Text                     as T
import qualified Options.Applicative           as A
import           System.FilePath                ( (</>)
                                                , takeDirectory
                                                )
import qualified System.FilePath.Glob          as Glob


--------------------------------------------------------------------------------
handleRun :: P.Conf -> User.UserName -> FilePath -> IO ExitCode
handleRun conf user scriptPath = do
  runtime <- Rt.boot conf >>= either throwIO pure
  code    <- interpret runtime user scriptPath
  Rt.powerdown runtime
  exitWith code

-- | Similar to `handleRun`, but capturing the output, and logging elsewhere
-- than normally: this is used in tests and in the `/scenarios` handler.
handleRun' :: FilePath -> IO [Trace]
handleRun' scriptPath = do
  let conf = P.Conf
        { P._confLogging = P.mkLoggingConf "/tmp/cty-serve-explore.log"
        , P._confDbFile  = Nothing
        }
  runtime <- Rt.boot conf >>= either throwIO pure
  output  <- interpretFile runtime "system" scriptPath 0
  Rt.powerdown runtime
  pure output

interpret :: Rt.Runtime -> User.UserName -> FilePath -> IO ExitCode
interpret runtime user path = do
  output <- interpretFile runtime user path 0
  let (exitCode, ls) = formatOutput output
  mapM_ putStrLn ls
  pure exitCode


--------------------------------------------------------------------------------
data Trace = Trace
  { traceNumber   :: Int -- ^ Flat numbering of commands within the script.
  , traceLineNbr  :: Int
  , traceCommand  :: Text
  , traceComment  :: Maybe Text
  , traceNesting  :: Int
  , traceUser     :: User.UserName
  , traceOutput   :: [Text]
  , traceExitCode :: ExitCode
  , traceNested   :: [Trace]
  , traceState    :: Data.HaskDb Rt.Runtime -- ^ The resulting state.
  }

-- | Keep all traces, but removes the `traceNested` indirection.
flatten :: [Trace] -> [Trace]
flatten [] = []
flatten (t:ts) = t { traceNested = [] } : flatten (traceNested t) ++ flatten ts

interpretFile :: Rt.Runtime -> User.UserName -> FilePath -> Int -> IO [Trace]
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
  -> IO [Trace]
interpret' runtime user dir content nesting = go user [] 0
  $ zip [1 :: Int ..] content
 where
  go _ acc _ []                  = pure acc
  go user' acc nbr ((ln, line) : rest) = do
    let (prefix, comment) = T.breakOn "#" line
        separated         = map T.pack . wordsq $ T.unpack prefix
        grouped           = T.unwords separated
        nbr'  = succ nbr
        trace' = Trace nbr
                       ln
                       grouped
                       (if T.null comment then Nothing else Just comment)
                       nesting
                       user'
    case separated of
      []               -> go user' acc nbr rest
      ["as", username] -> do
        st <- Rt.runRunM runtime Rt.state
        let t    = trace' ["Modifying default user."] ExitSuccess [] st
            acc' = acc ++ [t]
        go (User.UserName username) acc' nbr' rest
      ["quit"] -> do
        st <- Rt.runRunM runtime Rt.state
        let t    = trace' ["Exiting."] ExitSuccess [] st
            acc' = acc ++ [t]
        go user' acc' nbr' rest
      input -> do
        let result =
              A.execParserPure A.defaultPrefs Command.parserInfo
                $   T.unpack
                <$> input
        case result of
          A.Success command -> do
            case command of
              Command.Reset _ -> do
                Rt.runRunM runtime $ Rt.reset
                st <- Rt.runRunM runtime Rt.state
                let t = trace' ["Resetting to the empty state."] ExitSuccess [] st
                    acc' = acc ++ [t]
                go user' acc' nbr' rest
              Command.Run _ scriptPath -> do
                output' <- liftIO $ interpretFile runtime
                                                  user
                                                  (dir </> scriptPath)
                                                  (succ nesting)
                st <- Rt.runRunM runtime Rt.state
                let t    = trace' [] ExitSuccess output' st
                    acc' = acc ++ [t]
                go user' acc' nbr' rest
              _ -> do
                (_, output) <- Rt.handleCommand runtime user' command
                st <- Rt.runRunM runtime Rt.state
                let t    = trace' output ExitSuccess [] st
                    acc' = acc ++ [t]
                go user' acc' nbr' rest
          A.Failure err -> do
            st <- Rt.runRunM runtime Rt.state
            let t    = trace' [show err] (ExitFailure 1) [] st
                acc' = acc ++ [t]
            go user' acc' nbr' rest
          A.CompletionInvoked _ -> do
            st <- Rt.runRunM runtime Rt.state
            let t    = trace' ["Shouldn't happen."] (ExitFailure 1) [] st
                acc' = acc ++ [t]
            go user' acc' nbr' rest


--------------------------------------------------------------------------------
formatOutput :: [Trace] -> (ExitCode, [Text])
formatOutput output =
  let ls = concatMap showTrace output
      exitCode =
        if null output then ExitSuccess else traceExitCode $ last output
  in  (exitCode, ls)

showTrace :: Trace -> [Text]
showTrace Trace {..} =
  map (pad traceNesting <>)
    $  (show traceLineNbr <> ": " <> traceCommand)
    :  traceOutput
    ++ concatMap showTrace traceNested

pad 0 = ""
pad 1 = "> "
pad n = T.concat (replicate ((n - 1) * 2) ">") <> "> "


--------------------------------------------------------------------------------
listScenarios :: FilePath -> IO [FilePath]
listScenarios scenariosDir = sort <$> Glob.globDir1 pat scenariosDir
  where pat = Glob.compile "*.txt"


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

