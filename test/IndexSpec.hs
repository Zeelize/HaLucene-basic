module IndexSpec (spec) where

import Test.Hspec    

import IndexWriter
import IndexSearcher
    
spec :: Spec
spec = do
    describe "Basic index tests" $ do
        it "writer and searcher" $ do
            (5 + 6) `shouldBe` 11
