module Day01 where

import Data.List (find)
import Data.Either (isRight, fromLeft)
import Data.Set (Set, empty, insert, member)

input :: IO [Int]
input = fmap parseInt . lines <$> readFile "input/input01.txt"

parseInt :: String -> Int
parseInt = read . filter (/= '+')

solve1 = sum <$> input

solve2 = fromLeft undefined . head . dropWhile isRight <$> scanl foo (Right (empty :: Set Int)) <$> scanl1 (+) <$> concat . repeat <$> input
 where  foo (Left _) x   = Left x
        foo (Right xs) x = if x `member` xs then Left x else Right $ insert x xs

-- what is the resulting frequency
solution1 = solve1
-- 400

-- What is the first frequency your device reaches twice?
solution2 = solve2
-- 232