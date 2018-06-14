{-|
Module      : IndexSearcher
Description : Find and read created inverted indexes
Copyright   : (c) Vojtěch Mráz, 2018
License     : MIT
Maintainer  : mrazvoj1@fit.cvut.cz
Stability   : experimental
Portability : POSIX

Its possible to lookup the terms and user query in the inverted indexes created by IndexWriter.

Basic theory behind:
Lets say we have inverted indexes:
    Terms:
        hello-1;2;
        welcome-1;
        red-1;
        dwarf-1;
        world-2;
    Docs:
        1-red dwarf,5;
        2-hello word,2;

And user query is [hello], it will find both of the document, but it will return document number 2 like more relevant, 
because its lenght is smaller than size of the first document (If we will be someday store TF (term frequency) also,
we can compute it easily with the equation TF/DF, it means number of occcurences).

If we have longer user query, it will compute for every single term separately and after that it will sum the output.
Lets say we have user query [Hello red]. 
Relevancy for term [hello]:
    doc1 - 1/5
    doc2 - 1/2
Relevancy for term [red]:
    doc1 - 1/5
    doc2 - 0/2
Final relevancies:
    doc1 = 1/5 + 1/5
    doc2 = 1/2

EDIT #1: Because we are storing only term per document and not their frequencies, index searcher can do some mistakes. 
That is possible improvement, to add TF into IndexWriter.
-}
module IndexSearcher where

import Data.List
import System.IO.Unsafe

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S
import qualified StandardAnalyzer as SA

-- | Map of documents and number fo terms relevant for it
type TermsDocsMap = M.Map Int Int

-- | Find top N relevant documents for the user query and indexes on given path
findRelevantDocs :: Int -> String -> FilePath -> FilePath -> [(Double, T.Text)]
findRelevantDocs n query ifp dfp = do
    -- analyze user query
    let analQ = SA.analyze (T.pack query)
    -- getting relevant docs with tf
    let relDoc = preSearch analQ ifp
    -- merge relevant docs with docs index
    let docList = mergeDocs dfp relDoc
    -- sort list and return top N 
    docList

-- | Pre-proccess for index searcher
preSearch :: T.Text -> FilePath -> TermsDocsMap
preSearch q ifp = searcher qterms ti M.empty
    where 
        ti = T.lines . T.pack . unsafePerformIO $ readFile ifp
        qterms = S.fromList . nub . T.words $ q

-- | For all terms it will compute final document relevancy number 
searcher :: S.Set T.Text -> [T.Text] -> TermsDocsMap -> TermsDocsMap
searcher _ [] tdm = tdm
searcher q (x:xs) tdm 
    | S.null q = tdm
    | S.member term q = searcher nq xs ntdm
    | otherwise = searcher q xs tdm
    where
        splitted = T.splitOn (T.pack "-") x 
        term = head splitted 
        docs = head . tail $ splitted
        nq = S.delete term q
        ntdm = proccessTermLine tdm (T.splitOn (T.pack ";") docs)
        -- It will proccess line with docs and add them to do 
        proccessTermLine :: TermsDocsMap -> [T.Text] -> TermsDocsMap
        proccessTermLine tdm [] = tdm
        proccessTermLine tdm (x:xs) = proccessTermLine ntdm xs
            where
                docId = read $ T.unpack x :: Int
                ntdm = case M.member docId tdm of
                    True -> M.adjust (+ 1) docId tdm 
                    _ -> M.insert docId 1 tdm

-- | Merge relevant documents according to user query with docs index
mergeDocs :: FilePath -> TermsDocsMap -> [(Double, T.Text)]
mergeDocs dfp tdm = docMerger tdm di []
    where
        di = T.lines . T.pack . unsafePerformIO $ readFile dfp

-- | If document is relevant save it to final relevant documents
docMerger :: TermsDocsMap -> [T.Text] -> [(Double, T.Text)] -> [(Double, T.Text)]
docMerger _ [] relD = relD
docMerger tdm (x:xs) relD 
    | M.null tdm = relD
    | otherwise = case M.lookup docId tdm of
        Just tf -> docMerger ntdm xs (proccesDocLine relD (T.splitOn (T.pack ",") rest) tf)
        _ -> docMerger tdm xs relD
    where 
        splitted = T.splitOn (T.pack "-") x 
        docId = read . T.unpack . head $ splitted :: Int
        rest = head . tail $ splitted
        ntdm = M.delete docId tdm
        --nrelD = proccesDocLine relD (T.splitOn (T.pack ",") rest) tf
        -- It will add new relevant document to the final list with computed weight
        proccesDocLine :: [(Double, T.Text)] -> [T.Text] -> Int -> [(Double, T.Text)]
        proccesDocLine d [x, y] tf = d ++ [(weight, x)]
            where 
                len = read $ T.unpack y :: Int
                weight = (fromIntegral tf) / (fromIntegral len)
