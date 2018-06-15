import Test.Hspec

import qualified AutocorrectSpec
import qualified StandardAnalyzerSpec
import qualified IndexSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Autocorrect module"      AutocorrectSpec.spec
  describe "StandardAnalyzer module"      StandardAnalyzerSpec.spec
  describe "IndexWriter and IndexSearcher module"      IndexSpec.spec
