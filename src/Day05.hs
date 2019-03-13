{-# LANGUAGE TupleSections #-}
module Day05 where

import Control.Arrow ((&&&))
import Data.List (nub)
import Data.Char (toLower, toUpper)
--import Data.Sequence

input = readFile "input/input05.txt"

react a (_,[])                        = (False, [a])
react a (_,bs) |         a == head bs = (False, a : bs)
react a (_,bs) | toLower a == head bs = (True, tail bs)
react a (_,bs) | toUpper a == head bs = (True, tail bs)
react a (_,bs)                        = (False, a : bs)

act = foldr react (False,[]) . snd

solve1 = length . snd . head . dropWhile fst . iterate act . (True,)

-- How many units remain after fully reacting the polymer you scanned?
solution1 = solve1 <$> input
-- 10762

solve2 = minimum . fmap solve1 . (\(xs,inp) -> fmap (\x -> filter ((/= x) . toLower) inp) xs) . ((nub . fmap toLower) &&& id)

-- What is the length of the shortest polymer you can produce
solution2 = solve2 <$> input
-- 6946