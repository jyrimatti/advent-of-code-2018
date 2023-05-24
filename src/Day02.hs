module Day02 where

import           Control.Arrow ((&&&))
import           Data.List (group, intersect, sort, tails)
import           Data.Tuple.Extra (both)
import           Text.EditDistance (defaultEditCosts, levenshteinDistance)


input :: IO [String]
input = lines <$> readFile "input/input02.txt"

frequencies :: String -> [Int]
frequencies = fmap length . group . sort

relevantLetterAppearance :: String -> (Bool, Bool)
relevantLetterAppearance =         (elem 2 &&& elem 3) . filter (`elem` [2,3]) . frequencies
-- Same as:              = ((,) <$> elem 2 <*> elem 3) . filter (`elem` [2,3]) . frequencies

solve1 :: [String] -> Int
solve1 = uncurry (*) . both (length . filter (== True)) . unzip . fmap relevantLetterAppearance
-- uncurry (*) is same as: ((*) <$> fst <*> snd)

-- What is the checksum
solution1 :: IO Int
solution1 = solve1 <$> input
-- 5658


pairHeadWithOthers :: [a] -> [(a, a)]
--pairHeadWithOthers xs = zip (repeat (head xs)) (tail xs) -- pointful
--pairHeadWithOthers = ap (zip . repeat . head) tail       -- from pointfree.io
--pairHeadWithOthers = (zip . (repeat . head)) <*> tail    -- from CodeGPT
pairHeadWithOthers = zip <$> repeat . head <*> tail

pairings :: [String] -> [(String, String)]
pairings = concatMap pairHeadWithOthers . tails

smallestDistances :: [String] -> [(String, String)]
smallestDistances = filter ((== 1) . uncurry (levenshteinDistance defaultEditCosts)) . pairings

solve2 :: [String] -> String
solve2 = uncurry intersect . head . smallestDistances

-- What letters are common between the two correct box IDs?
solution2 :: IO String
solution2 = solve2 <$> input
-- nmgyjkpruszlbaqwficavxneo