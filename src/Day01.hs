module Day01 where

import Control.Arrow ((&&&))
import Control.Monad (when)
import Data.Either   (fromLeft, isRight)
import Data.List     (find)
import Data.Maybe    (catMaybes, fromJust, isJust, isNothing,
                                mapMaybe)
import Data.Set      (Set, elemAt, empty, insert, lookupIndex, member)
import Util



input :: IO [Int]
input = fmap parseInt . lines <$> readFile "input/input01.txt"

parseInt :: String -> Int
parseInt = read . filter (/= '+')

-- what is the resulting frequency
solution1 = sum <$> input
-- 400


findElem :: Int -> Set Int -> Maybe Int
findElem = fmap . const <$$>> arg1 <*< lookupIndex

markIfContains :: Int -> Set Int -> (Maybe Int, Set Int)
markIfContains = (,) <$$>> findElem <*< insert

solve2 :: [Int] -> Int
solve2 = head . mapMaybe fst . scanl (flip markIfContains . snd) (Nothing,empty) . scanl1 (+) . cycle

-- What is the first frequency your device reaches twice?
solution2 = solve2 <$> input
-- 232
