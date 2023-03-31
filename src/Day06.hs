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
    ( (<$$$$>>),
      (<$$$$>>>>),
      (<$$>>),
      (<$$>>>),
      (<*<),
      anyOf,
      arg41,
      arg42,
      arg43,
      arg44 )

input :: IO [String]
input = lines <$> readFile "input/input06.txt"

type Parser = Parsec () String

type Coordinate = (Int,Int)

coordinateP :: Parser Coordinate
coordinateP = (,) <$> (decimal <* string ", ") <*> decimal

coordinate :: String -> Coordinate
coordinate = fromJust . parseMaybe coordinateP

manhattan :: Coordinate -> Coordinate -> Int
--manhattan (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)
manhattan = ((+) `on` abs) <$$>> ((-) `on` fst)
                             <*< ((-) `on` snd)

relevantWorld :: [Coordinate] -> (Coordinate, Coordinate)
--relevantWorld = (minimum . fmap fst &&& minimum . fmap snd) &&& (maximum . fmap fst &&& maximum . fmap snd)
relevantWorld = ((,) <$> both minimum <*> both maximum) . unzip

-- "simple and easy" way to say: x==x1||x==x2||y==y1||y==y2
comparators :: (Int, Int) -> (Int, Int) -> Int -> Int -> Bool
comparators = anyOf <$$$$>>>> ((==) <$$$$>> arg43 <*< fst ... arg41) -- x == x1
                          <*< ((==) <$$$$>> arg43 <*< fst ... arg42) -- x == x2
                          <*< ((==) <$$$$>> arg44 <*< snd ... arg41) -- y == y1
                          <*< ((==) <$$$$>> arg44 <*< snd ... arg42) -- y == y2

allRelevantCoordinates :: Coordinate -> Coordinate -> [(Coordinate, Bool)]
--allRelevantCoordinates (x1,y1) (x2,y2) = [((x,y),x==x1||x==x2||y==y1||y==y2) | x <- [x1..x2], y <- [y1..y2]]
-- ugh, too difficult...
allRelevantCoordinates = liftA2 . ((,) <$$>> (,) <*<) <$$>>> comparators <*< (enumFromTo `on` fst) <*< (enumFromTo `on` snd)

pairwiseDistancesTo :: [Coordinate] -> [(Coordinate, Bool)] -> [([Int], Bool)]
--pairwiseDistancesTo cs ccs = fmap (first $ (flip fmap cs) . manhattan) ccs
pairwiseDistancesTo = fmap . first . (. manhattan) . flip fmap -- aaargh...

removeElements :: [Int] -> [(Int, Bool)] -> [(Int, Bool)]
removeElements = filter . (. fst) . flip notElem -- aaargh...

havingExactMinimum :: ([Int], Bool) -> Bool
havingExactMinimum = (== 1) . length . head . group . sort . fst

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
solution1 :: IO Int
solution1 = solve1 <$> input
-- 5035


solve2 :: [String] -> Int
solve2 = length . filter (<10000) . fmap (sum . fst) . distances

-- What is the size of the region containing all locations which have a total distance to all given coordinates of less than 10000?
solution2 :: IO Int
solution2 = solve2 <$> input
-- 35294