{-|
Module      : IndexWriter
Description : Creates Inverted Index of terms and documents
Copyright   : (c) Vojtěch Mráz, 2018
License     : MIT
Maintainer  : mrazvoj1@fit.cvut.cz
Stability   : experimental
Portability : POSIX

Creates inverted index for given terms and documents. 
It can only write a new index (it is not possible to merge or append new documents, with that, we have to index everything again).

Theory behind it:
    It will take list of documents (texts) and tokenize it into terms (words). 
    After that it will create map of terms with key = term and value is pair of docId and tf (term frequency) in the doc.
    Also it will create map of documents with key = docID and value is pair of name of doc and length of the doc.
    
    At the end, it will create index files for Index Searcher to use it. With that, first indexing can be quite long,
    but search for the query later will be fast and it is not neccessary to index documents again.

Example of inverted index for document [Red Dwarf] with text [Hello, welcome on red dwarf!] and [Hello word] with text [Hello world.]:
    Terms:
        hello-1,1;2,1;
        welcome-1,1;
        red-1,1;
        dwarf-1,1;
        world-2,1;
    Docs:
        1-red dwarf,5;
        2-hello word,2;

EDIT #1: Terms Index is storing only ids of documents in which they occur:
    Terms:
        hello-1;2;
        welcome-1;
        red-1;
        dwarf-1;
        world-2;
-}
module IndexWriter where

import Data.List

import qualified Data.Map as M
import qualified Data.Text as T
import qualified StandardAnalyzer as SA

-- | Map of terms with key of text and value of docId and tf in doc
type TermsIndex = M.Map T.Text [Int]

-- | Map of docs with key of docId and value pair of name and length
type DocsIndex = M.Map Int (T.Text, Int)

-- | Main function to call for creating indexes
-- input is list of documents -> pair of name and content
writeIndex :: [(T.Text, T.Text)] -> FilePath -> FilePath -> IO ()
writeIndex docs tifp difp = do
    let (ti, di) = procDocuments docs M.empty M.empty 0
    -- Write maps into a files
    scoreTI <- storeTermsIndex (M.toList ti) tifp
    --scoreDI <- storeDocsIndex (M.toList di) difp
    return ()

-- | Create index and stores it into maps for documents
procDocuments :: [(T.Text, T.Text)] -> TermsIndex -> DocsIndex -> Int -> (TermsIndex, DocsIndex)
procDocuments [] ti di _ = (ti, di)
procDocuments (x:xs) ti di i = procDocuments xs nti nid (i + 1)
    where 
        analX = SA.analyze (snd x)
        nid = addDoc i (fst x) (length . T.words $ analX) di
        nti = addTerms i ti analX

-- | Add document to docMap
addDoc :: Int -> T.Text -> Int -> DocsIndex -> DocsIndex
addDoc i name n di = M.insert i (name, n) di 

-- | Add terms into termsMap
addTerms :: Int -> TermsIndex -> T.Text -> TermsIndex
addTerms docId ti text = addTerm docId ti tList
    where 
        tList = nub . T.words $ text

-- | Add one specific term to term map
addTerm :: Int -> TermsIndex -> [T.Text] -> TermsIndex
addTerm _ ti [] = ti
addTerm docId ti (x:xs) = addTerm docId nti xs
    where 
        nti = case M.member x ti of
            True -> M.adjust (++ [docId]) x ti 
            _ -> M.insert x [docId] ti
             
-- | Store TermsIndex map into file
storeTermsIndex :: [(T.Text, [Int])] -> FilePath -> IO ()
storeTermsIndex [] _ = return ()
storeTermsIndex (x:xs) fp = do
    appendFile fp $ T.unpack (fst x) ++ "-" ++ indexToString (snd x)
    storeTermsIndex xs fp
    where
        indexToString :: [Int] -> String
        indexToString [d] = show d
        indexToString (d:dr) = show d ++ ";" ++ indexToString dr 

-- | Store DocsIndex map into file
--storeDocsIndex :: [(Int, (T.Text, Int))] -> FilePath -> IO ()
