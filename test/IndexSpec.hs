module IndexSpec (spec) where

import Test.Hspec    
import System.IO.Unsafe
import Control.Monad
import System.Directory

import qualified Data.Text as T

import qualified IndexWriter as IW
import qualified IndexSearcher as IS
    
-- | It is important before testing to delete index files
writeAndFind :: IO ([(Double, T.Text)])
writeAndFind = do
    -- delete if exists
    texists <- doesFileExist "terms.pidx"
    when texists (removeFile "terms.pidx")
    dexists <- doesFileExist "docs.pidx"
    when dexists (removeFile "docs.pidx")
    -- write
    wr <- IW.writeIndex [((T.pack "Red Dwarf"), (T.pack "Hello, welcome on the hello Red Dwarf! We have red chairs, small dwarfs")), ((T.pack "Coding"), (T.pack "To write some code, start with hello. And then continue coding with writing code about dwarfs."))] "terms.pidx" "docs.pidx"
    -- find relevant
    IS.findRelevantDocs 5 "Hello! You should code red dwarf!" "terms.pidx" "docs.pidx"

spec :: Spec
spec = do
    describe "Basic index tests" $ do
        it "writer and searcher" $ do
            (unsafePerformIO writeAndFind) `shouldBe` [(0.6666666666666666,T.pack "Red Dwarf"),(0.5555555555555556,T.pack "Coding")]
