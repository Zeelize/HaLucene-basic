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
import Control.Monad
import System.Directory

import qualified Data.Text as T

import Autocorrect (spell)
import IndexSearcher (findRelevantDocs)
import IndexWriter (writeIndex)

-- First we load our data - create Index - and for user query find relevant documents
main :: IO ()
main = do
    -- if not created index, load csv and create one
    -- DISCLAIMER!: we are only checking for terms.pidx, of course only for example purposes
    index <- doesFileExist "app/terms.pidx"
    when (not index) (createIndexes)
    -- ask for user query in loop and return relevant docs
    putStrLn "Welcome to search engine for Home-Depot products."
    putStrLn "[HELP]: if you want to quit searching, write \":quit\" instead of your search query!"
    communicator

-- Communicator with user
communicator :: IO ()
communicator = do
    putStrLn "\nSearch: "
    query <- getLine
    putStrLn "Number of documents: "
    num <- getLine
    case query == ":quit" of
        True -> putStrLn "Thank you for using Home-depot search engine."
        _ -> do
            -- search query
            putStrLn . show . unsafePerformIO $ findRelevantDocs (read num) query "app/terms.pidx" "app/docs.pidx"
            -- run communicator again
            communicator

-- Load CSV File and create index
createIndexes :: IO ()
createIndexes = do
    -- Load data
    let products = loadCSVFile "app/home_depot.csv"
    -- create index if not exists
    writeIndex products "app/terms.pidx" "app/docs.pidx"

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
