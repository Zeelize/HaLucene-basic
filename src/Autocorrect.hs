{-|
Module      : Autocorrect
Description : Module for autoccorecting spelling mistakes (up to mistakes of second order)
Copyright   : (c) Vojtěch Mráz, 2018
License     : MIT
Maintainer  : mrazvoj1@fit.cvut.cz
Stability   : experimental
Portability : POSIX

Autocorrecter based on Norvig's algorithm (https://norvig.com/spell-correct.html)
Module for autoccorecting spelling mistakes (up to mistakes of second order). 
Mistakes of types delete/transpose/replace/insert.
-}
module Autocorrect where

import Prelude
import Data.List
import Data.Char
import Data.Maybe
import qualified Data.Map.Strict as M
import Data.List.Split (splitWhen)
import Data.List.Extras.Argmax (argmax)
import System.IO.Unsafe

-- | Simulate all words from our dictionary provided by Norvig
-- we need only words (isAlpha) and then we are filtering array which are empty (not words from alphabet)
-- and also converting all words to lower case
{-# NOINLINE ws #-}
ws :: M.Map String Int
ws = M.fromList [(head l, length l) | l <- group (sort pt) ]
    where 
        text = unsafePerformIO $ readFile "big.txt" 
        pt = filter (not . null) . splitWhen (not . isAlpha) $ map toLower text

-- | Computing a probability of a word - how often it was used in our dictionary
-- n is a number of all words in our dictionary (we are summing up all the occurences)
prob :: String -> Double
prob w = (fromIntegral $ fromMaybe 0 (M.lookup w ws :: Maybe Int)) / n
    where 
        --n = fromIntegral $ M.size ws 
        n = fromIntegral $ M.foldl' (+) 0 ws 

-- | Find the best possible candidate from all canditates for word
spell :: String -> String
spell w = argmax prob $ candidates w

-- | Find all candidates for given word
candidates :: String -> [String]
candidates w = head $ filter (not . null) mc
    where 
        mc = [known [w], known (edFirstOrder w), known (edSecOrder w), [w]]
    
-- | Find the set of words that appears in our dictionary and they are therefor known for us
-- Argument is list of possible candidates and we return only those which are known
known :: [String] -> [String]
known posW = [w | w <- posW, M.member w ws]

-- | All mistakes of first order
-- Function is computing one of the possible mistake of the type delete/transpose/replace/insert
-- We can replace original python code from Norvig with just Haskell rules (implementation)
edFirstOrder :: String -> [String]
edFirstOrder w = deletes ++ transposes ++ replaces ++ inserts
    where
        -- alphabet
        alpha = ['a' .. 'z']
        -- all possible splits of the the word 
        splits = [splitAt i w | i <- [1 .. length w]]
        deletes = [l ++ tail r | (l,r) <- splits, (not . null) r]
        transposes = [l ++ r !! 1 : head r : drop 2 r | (l,r) <- splits, length r > 1]
        replaces = [l ++ c : tail r | (l,r) <- splits, (not . null) r, c <- alpha]
        inserts = [l ++ c : r | (l,r) <- splits, c <- alpha]

-- | All mistakes and edits of second order
-- Computes first order mistakes and for all results, it will run again
edSecOrder :: String -> [String]
edSecOrder w = [e2 | e1 <- edFirstOrder w, e2 <- edFirstOrder e1]

