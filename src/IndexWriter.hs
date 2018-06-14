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
-}
module IndexWriter where

import Data.List

import qualified Data.Map as M
import qualified Data.Text as T
import qualified StandardAnalyzer as SA

-- | Map of terms with key of text and value of docId and tf in doc
type TermsIndex = M.Map T.Text (Int, Int)

-- | Map of docs with key of docId and value pair of name and length
type DocsIndex = M.Map Int (T.Text, Int)

-- | Main function to call for creating indexes
-- input is list of documents -> pair of name and content
writeIndex :: [(T.Text, T.Text)] -> IO ()
writeIndex docs = do
    let (ti, di) = procDocuments docs M.empty M.empty 0
    -- Write maps into a files
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
addTerms docId ti text = ti
