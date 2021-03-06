module Day06 where

import Control.Applicative            ( liftA2 )
import Control.Arrow              ((&&&))
import Data.Bifunctor             (bimap)
import Data.Function              (on)
import Data.List                  (find, group, maximumBy, minimumBy,
                                             nub, null, sort)
import Data.List.Extra            (minimumOn)
import Data.Maybe (fromJust)
import Data.Tuple.Extra           (both, first, second)
import Text.Megaparsec            (Parsec, anySingleBut, many,
                                             optional, parseMaybe, try, (<|>))
import Text.Megaparsec.Char       (char, letterChar, space, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Universum.VarArg               ( (...) )
import Util

input = lines <$> readFile "input/input06.txt"

type Parser = Parsec () String

type Coordinate = (Int,Int)

coordinateP :: Parser Coordinate
coordinateP = (,) <$> (decimal <* string ", ") <*> decimal

coordinate :: String -> Coordinate
coordinate = fromJust . parseMaybe coordinateP

manhattan :: Coordinate -> Coordinate -> Int
--manhattan (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)
manhattan = (+) `oN` abs <$$>> (-) `oN` fst <*< (-) `oN` snd

relevantWorld :: [Coordinate] -> (Coordinate, Coordinate)
--relevantWorld = (minimum . fmap fst &&& minimum . fmap snd) &&& (maximum . fmap fst &&& maximum . fmap snd)
relevantWorld = (,) <$> both minimum . unzip <*> both maximum . unzip

-- "simple and easy" way to say: x==x1||x==x2||y==y1||y==y2
comparators :: (Int, Int) -> (Int, Int) -> Int -> Int -> Bool
comparators = anyOf <$$$$>>>> ((==) <$$$$>> arg43 <*< fst ... arg41)
                          <*< ((==) <$$$$>> arg43 <*< fst ... arg42)
                          <*< ((==) <$$$$>> arg44 <*< snd ... arg41)
                          <*< ((==) <$$$$>> arg44 <*< snd ... arg42)

allRelevantCoordinates :: Coordinate -> Coordinate -> [(Coordinate, Bool)]
--allRelevantCoordinates (x1,y1) (x2,y2) = [((x,y),x==x1||x==x2||y==y1||y==y2) | x <- [x1..x2], y <- [y1..y2]]
-- ugh, too difficult...
allRelevantCoordinates = liftA2 . ((,) <$$>> (,) <*<) <$$>>> comparators <*< enumFromTo `oN` fst <*< enumFromTo `oN` snd

pairwiseDistancesTo :: [Coordinate] -> [(Coordinate, Bool)] -> [([Int], Bool)]
pairwiseDistancesTo = fmap . first . (. manhattan) . flip fmap -- aaargh...

removeElements :: [Int] -> [(Int, Bool)] -> [(Int, Bool)]
removeElements = filter . (. fst) . flip notElem -- aaargh...

havingExactMinimum :: ([Int], Bool) -> Bool
havingExactMinimum = ((== 1) . length) . head . group . sort . fst

indexOfMinimum :: [Int] -> Int
indexOfMinimum = fst . minimumOn snd . zip [0..]

gatherInfinite :: [(Int, Bool)] -> [Int]
gatherInfinite = fmap fst . filter snd

largestArea :: [(Int, Bool)] -> Int
largestArea = maximum . fmap length . group . sort . fmap fst

distances :: [String] -> [([Int], Bool)]
distances = (pairwiseDistancesTo <$> id <*> uncurry allRelevantCoordinates . relevantWorld) . fmap coordinate

solve1 :: [String] -> Int
solve1 = largestArea . (removeElements <$> gatherInfinite <*> id) . fmap (first indexOfMinimum) . filter havingExactMinimum . distances

-- What is the size of the largest area that isn't infinite?
solution1 = solve1 <$> input
-- 5035


solve2 :: [String] -> Int
solve2 = length . filter (<10000) . fmap (sum . fst) . distances

-- What is the size of the region containing all locations which have a total distance to all given coordinates of less than 10000?
solution2 = solve2 <$> input
-- 35294