module Day02 where

import Control.Arrow ((&&&))
import Data.Foldable (foldMap)
import Data.List (sort,group,nub,find,intersect, tails)
import Data.Tuple.Extra (both)
import Text.EditDistance (defaultEditCosts, levenshteinDistance)

input = lines <$> readFile "input/input02.txt"

frequencies :: String -> [Int]
frequencies = fmap length . group . sort

relevantLetterAppearance :: String -> (Bool, Bool)
relevantLetterAppearance = (elem 2 &&& elem 3) . nub . filter (`elem` [2,3]) . frequencies

solve1 :: [String] -> Int
solve1 = uncurry (*) . both (length . filter id) . unzip . fmap relevantLetterAppearance

-- What is the checksum
solution1 = solve1 <$> input
-- 5658


pairings :: [String] -> [(String, String)]
pairings xs = [(a, b) | a:bs <- tails xs, b <- bs]

smallestDistances :: [String] -> [(String, String)]
smallestDistances = filter ((== 1) . uncurry (levenshteinDistance defaultEditCosts)) . pairings

solve2 :: [String] -> String
solve2 = uncurry intersect . head . smallestDistances

-- What letters are common between the two correct box IDs?
solution2 = solve2 <$> input
-- nmgyjkpruszlbaqwficavxneo