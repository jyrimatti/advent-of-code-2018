module Day06 where

import Control.Arrow ((&&&))
import Data.Bifunctor (bimap)
import Data.Function (on)
import Data.List (minimumBy,maximumBy,sort,group,nub,find,null)
import Data.List.Extra (minimumOn)
import Data.Tuple.Extra (first,second,both)
import Text.Megaparsec (Parsec,parse,optional,(<|>),try,many)
import Text.Megaparsec.Char (char,space,string,anyChar,letterChar,notChar)
import Text.Megaparsec.Char.Lexer (decimal,signed)

input = lines <$> readFile "input/input06.txt"

type Parser = Parsec () String

type Coordinate = (Int,Int)

coordinateP :: Parser Coordinate
coordinateP = (,) <$> (decimal <* string ", ") <*> decimal

coordinate :: String -> Coordinate
coordinate = either undefined id . parse coordinateP ""

manhattan :: Coordinate -> Coordinate -> Int
manhattan (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

relevantWorld :: [Coordinate] -> (Coordinate, Coordinate)
relevantWorld = (minimum . fmap fst &&& minimum . fmap snd) &&& (maximum . fmap fst &&& maximum . fmap snd)

allRelevantCoordinates :: (Coordinate, Coordinate) -> [(Coordinate, Bool)]
allRelevantCoordinates ((x1,y1),(x2,y2)) = [((x,y),x==x1||x==x2||y==y1||y==y2) | x <- [x1..x2], y <- [y1..y2]]

pairwiseDistancesTo :: [Coordinate] -> [(Coordinate, Bool)] -> [([Int], Bool)]
pairwiseDistancesTo coords = fmap (first $ (`fmap` coords) . manhattan)

removeElements :: [Int] -> [(Int, Bool)] -> [(Int, Bool)]
removeElements xs = filter ((`notElem` xs) . fst)

havingExactMinimum :: ([Int], Bool) -> Bool
havingExactMinimum = all ((== 1) . length) . take 1 . group . sort . fst

indexOfMinimum :: [Int] -> Int
indexOfMinimum = fst . minimumOn snd . zip [0..]

gatherInfinite :: [(Int, Bool)] -> [Int]
gatherInfinite = fmap fst . filter snd

largestArea :: [(Int, Bool)] -> Int
largestArea = maximum . fmap length . group . sort . fmap fst

distances :: [String] -> [([Int], Bool)]
distances = (pairwiseDistancesTo <$> id <*> allRelevantCoordinates . relevantWorld) . fmap coordinate

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