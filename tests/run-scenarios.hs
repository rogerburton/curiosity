-- | Run scripts similarly to `cty run`, ensuring their outputs are identical
-- to "golden" (expected) results.
import qualified Curiosity.Parse               as P
import qualified Curiosity.Run                 as Run
import qualified Curiosity.Runtime             as Rt
import qualified Data.Text                     as T
import           System.FilePath
import qualified System.FilePath.Glob          as Glob

import           Test.Tasty
import qualified Test.Tasty.Silver             as Silver

--------------------------------------------------------------------------------
main :: IO ()
main = do
  goldens <- listScenarios >>= mapM mkGoldenTest
  defaultMain $ testGroup "Tests" goldens


--------------------------------------------------------------------------------
listScenarios :: IO [FilePath]
listScenarios = sort <$> Glob.globDir1 pat "scenarios/"
  where pat = Glob.compile "*.txt"

mkGoldenTest :: FilePath -> IO TestTree
mkGoldenTest path = do
  let testName   = takeBaseName path
  let goldenPath = replaceExtension path ".golden"
  pure $ Silver.goldenVsAction testName goldenPath action convert
 where
  action :: IO [Text]
  action = do
    actual <- run path
    return actual

  convert = T.unlines

-- Similar to Run.handleRun, but capturing the output.
run :: FilePath -> IO [Text]
run scriptPath = do
  runtime <- Rt.boot P.defaultConf >>= either throwIO pure
  output  <- Run.interpretFile runtime "system" scriptPath 0
  Rt.powerdown runtime
  pure $ map (\(_ ,_ , c) -> c)  output
