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
findRelevantDocs :: Int -> String -> FilePath -> FilePath -> [T.Text]
findRelevantDocs n query ifp dfp = do
    -- analyze user query
    let analQ = SA.analyze (T.pack query)
    let relDoc = preSearch analQ ifp
    -- sort map and return top N 
    []

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
        ntdm = proccessDocLine tdm (T.splitOn (T.pack ";") docs)
        -- It will proccess line with docs and add them to do 
        proccessDocLine :: TermsDocsMap -> [T.Text] -> TermsDocsMap
        proccessDocLine tdm line = tdm
    