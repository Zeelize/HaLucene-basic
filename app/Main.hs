{-|
Module      : Main
Description : HaLucene library example
Copyright   : (c) Vojtěch Mráz, 2018
License     : MIT
Maintainer  : mrazvoj1@fit.cvut.cz
Stability   : experimental
Portability : POSIX

Short example how to use Halucene library for product in a eshop.

We have CSV file with product titles and product description and we are trying to find most relevant products according to
their descriptions and user query.
-}
module Main where

import System.IO.Unsafe

import qualified Data.Text as T

import Autocorrect (spell)
import IndexSearcher (findRelevantDocs)
import IndexWriter (writeIndex)

-- First we load our data - create Index - and for user query find relevant documents
main :: IO ()
main = do
    -- Load data
    let products = loadCSVFile "home_depot.csv"
    -- create index if not exists
    -- ask for user query in loop and return relevant docs
    putStrLn "HaLucene example..."

-- Load CSV file with products
loadCSVFile :: FilePath -> [(T.Text, T.Text)]
loadCSVFile fp = saveProduct lines []
    where
        lines = T.lines . T.pack . unsafePerformIO $ readFile fp 
        saveProduct :: [T.Text] -> [(T.Text, T.Text)] -> [(T.Text, T.Text)]
        saveProduct [] p = p
        saveProduct (x:xs) p = saveProduct xs np
            where 
                split = T.splitOn (T.pack ";") x 
                title = head split
                desc = head . tail $ split
                np = p ++ [(title, desc)]
