module Day01 where

import Control.Arrow ((&&&))
import Control.Monad (when)
import Data.Either   (fromLeft, isRight)
import Data.List     (find)
import Data.Maybe    (catMaybes, fromJust, isJust, isNothing,
                                mapMaybe)
import Data.Set      (Set, elemAt, empty, insert, lookupIndex, member)
import Util

type Elem = Int

input :: IO [Elem]
input = fmap parseElem . lines <$> readFile "input/input01.txt"

parseElem :: String -> Elem
parseElem = read . filter (/= '+')

-- what is the resulting frequency
solution1 = sum <$> input
-- 400


-- uuh, this is horible...
-- lookupIndex returns the _index_ of elem if it's there, but we wish to return Maybe Elem instead of Maybe Index
findElem :: Elem -> Set Elem -> Maybe Elem
findElem = fmap . const <$$>> arg1 <*< lookupIndex

markIfContains :: Elem -> Set Elem -> (Maybe Elem, Set Elem)
markIfContains = (,) <$$>> findElem <*< insert

solve2 :: [Elem] -> Elem
solve2 = head . mapMaybe fst . scanl (flip markIfContains . snd) (Nothing,empty) . scanl1 (+) . cycle

-- What is the first frequency your device reaches twice?
solution2 = solve2 <$> input
-- 232
