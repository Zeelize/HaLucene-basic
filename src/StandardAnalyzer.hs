{-|
Module      : StandardAnalyzer
Description : StandardAnalyzer for HaLucene with deleting stop-words and basic stemming
Copyright   : (c) Vojtěch Mráz, 2018
License     : MIT
Maintainer  : mrazvoj1@fit.cvut.cz
Stability   : experimental
Portability : POSIX

Standard Analyzer for proccesing texts provide deleting stop words such as (a, the, like, than, then, if, or etc...)
Also stemming words (runing - run, economic - econom etc...)
-}
module StandardAnalyzer where

import qualified Data.Text as T

import qualified English.StopWords as SW

-- | Transform all words into lower-case
textToLower :: T.Text -> T.Text
textToLower text = T.toLower text

-- | Delete all stopwords from the text
deleteStopWords :: T.Text -> T.Text
deleteStopWords text = T.unwords [w | w <- (T.words text), (not . SW.checkStopWord) w] 
