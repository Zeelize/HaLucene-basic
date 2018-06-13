module StandardAnalyzerSpec (spec) where

import Test.Hspec    
import qualified Data.Text as T

import StandardAnalyzer
    
spec :: Spec
spec = do
    describe "Basic operations of" $ do
        it "filtering punctuation" $ do
            erasePunc (T.pack "there, over the rainbow") `shouldBe` (T.pack "there over the rainbow")
            erasePunc (T.pack "Where it is?") `shouldBe` (T.pack "Where it is")
            erasePunc (T.pack "part 3: love") `shouldBe` (T.pack "part 3 love")
            erasePunc (T.pack "he said: \"hello!\"") `shouldBe` (T.pack "he said hello")
            erasePunc (T.pack "Done. Thankfully.") `shouldBe` (T.pack "Done Thankfully")
            erasePunc (T.pack "code delimeter is used like return a;") `shouldBe` (T.pack "code delimeter is used like return a")
        it "deleting stopwords" $ do
            deleteStopWords (T.pack "to create this paper") `shouldBe` (T.pack "create paper")
            deleteStopWords (T.pack "the me and myself tonight dance") `shouldBe` (T.pack "tonight dance")
            deleteStopWords (T.pack "how all any both of you") `shouldBe` (T.pack "")
        it "stemming words" $ do
            stem (T.pack "going") `shouldBe` (T.pack "go")
            stem (T.pack "gone") `shouldBe` (T.pack "gone")
            stem (T.pack "goes") `shouldBe` (T.pack "goe")
            stem (T.pack "went") `shouldBe` (T.pack "went")
            stem (T.pack "studying") `shouldBe` (T.pack "studi")
            stem (T.pack "study") `shouldBe` (T.pack "studi")
            stem (T.pack "studies") `shouldBe` (T.pack "studi")
            stem (T.pack "studied") `shouldBe` (T.pack "studi")
            stem (T.pack "considerable") `shouldBe` (T.pack "consider")
            stem (T.pack "analysis") `shouldBe` (T.pack "analysi")
            stem (T.pack "features") `shouldBe` (T.pack "featur")
        it "stemming queries" $ do
            stemText (T.pack "sending explanatory") `shouldBe` (T.pack "send explanatori")
            stemText (T.pack "create expands considerable") `shouldBe` (T.pack "creat expand consider")
    describe "Analyzing text" $ do
        it "short text" $ do
            analyze (T.pack "To create these eBooks, the Project expends considerable") `shouldBe` (T.pack "creat ebook project expend consider")
            analyze (T.pack "and its trustees and agents, and any volunteers associated") `shouldBe` (T.pack "truste agent volunt associ")
        it "long text" $ do
            analyze (T.pack "Do you really think it is weakness that yields to temptation? I tell you that there are terrible temptations, which it requires strength strength and courage to yield to - Oscar Wilde") `shouldBe` (T.pack "realli think weak yield temptat tell terribl temptat requir strength strength courag yield oscar wild")

