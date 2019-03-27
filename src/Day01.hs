module Day01 where

import Data.Either (isRight, fromLeft)
import Data.List (find)
import Data.Maybe (catMaybes, fromJust, isNothing, isJust, mapMaybe)
import Data.Set (Set, empty, insert, member, elemAt, lookupIndex)
import Control.Arrow ((&&&))

input :: IO [Int]
input = fmap parseInt . lines <$> readFile "input/input01.txt"

parseInt :: String -> Int
parseInt = read . filter (/= '+')

-- what is the resulting frequency
solution1 = sum <$> input
-- 400


findElem :: Int -> Set Int -> Maybe Int
findElem x = fmap . flip elemAt <$> id <*> lookupIndex x

markIfContains :: (Maybe Int, Set Int) -> Int -> (Maybe Int, Set Int)
markIfContains (_,set) = (`findElem` set) &&& (`insert` set)

solve2 :: [Int] -> Int
solve2 = head . mapMaybe fst . scanl markIfContains (Nothing,empty) . scanl1 (+) . cycle

-- What is the first frequency your device reaches twice?
solution2 = solve2 <$> input
-- 232
