import Test.Hspec

import qualified AutocorrectSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Autocorrect module"      AutocorrectSpec.spec
