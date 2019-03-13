module Day02 where

import Data.Tuple.Extra (both)
import Data.Foldable (foldMap)
import Data.Monoid
import Data.List (sort,group,nub,find,intersect)
import Text.EditDistance (defaultEditCosts, levenshteinDistance)

input = lines <$> readFile "input/input02.txt"

letterCounts = fmap length . group . sort

elemNum True = 1
elemNum False = 0

relevantLetterAppearance = (\xs -> (elemNum $ elem 2 xs, elemNum $ elem 3 xs)) . nub . filter (\x -> x == 2 || x == 3) . letterCounts

checksum = uncurry (*) . both getSum . foldMap (both Sum) . fmap relevantLetterAppearance

solve1 = checksum <$> input

smallestDistances xs = [(a, b) | a <- xs, b <- xs, levenshteinDistance defaultEditCosts a b == 1]

solve2 = uncurry intersect . head . smallestDistances <$> input

-- What is the checksum
solution1 = solve1
-- 5658

-- What letters are common between the two correct box IDs?
solution2 = solve2
-- nmgyjkpruszlbaqwficavxneo