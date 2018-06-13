import Test.Hspec

import qualified AutocorrectSpec
import qualified StandardAnalyzerSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Autocorrect module"      AutocorrectSpec.spec
  describe "StandardAnalyzer module"      StandardAnalyzerSpec.spec
