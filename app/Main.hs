module Main where

import Autocorrect (spell)
import IndexSearcher (findRelevantDocs)
import IndexWriter (writeIndex)

-- | Works like a example of using HaLucene library
-- First we load our data - create Index - and for user query find relevant documents
main :: IO ()
main = do
    -- Load data
    -- create index if not exists
    -- ask for user query in loop and return relevant docs
    putStrLn "HaLucene example..."
