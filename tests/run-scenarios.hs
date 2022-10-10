-- | Run scripts similarly to `cty run`, ensuring their outputs are identical
-- to "golden" (expected) results.
import qualified Curiosity.Interpret           as Inter
import qualified Data.Text                     as T
import           System.FilePath

import           Test.Tasty
import qualified Test.Tasty.Silver             as Silver

--------------------------------------------------------------------------------
main :: IO ()
main = do
  goldens <- Inter.listScenarios "scenarios/" >>= mapM mkGoldenTest
  defaultMain $ testGroup "Tests" goldens


--------------------------------------------------------------------------------
mkGoldenTest :: FilePath -> IO TestTree
mkGoldenTest path = do
  let testName   = takeBaseName path
  let goldenPath = replaceExtension path ".golden"
  pure $ Silver.goldenVsAction testName goldenPath action convert
 where
  action :: IO [Text]
  action = snd . Inter.formatOutput <$> Inter.handleRun' path

  convert = T.unlines
