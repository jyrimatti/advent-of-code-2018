{-# LANGUAGE TupleSections #-}
module Day03 where

import           Control.Arrow ((&&&))
import           Data.List (sort,group,groupBy,sortBy,find)
import           Data.List.Extra (groupSortOn)
import Data.Tuple.Extra (snd3, thd3)
import           Data.Maybe (fromJust)
import qualified Data.Set as Set (fromList)
import           Data.Set (notMember)
import           Text.Megaparsec (Parsec,parse,optional,(<|>),try,many)
import           Text.Megaparsec.Char (char,space,string,anyChar,letterChar)
import           Text.Megaparsec.Char.Lexer (decimal,signed)

input = lines <$> readFile "input/input03.txt"
data Claim = Claim {
  claimId :: Int,
  x       :: Int,
  y       :: Int,
  width   :: Int,
  height  :: Int
} deriving Show

type Parser = Parsec () String

claimP :: Parser Claim
claimP = Claim <$> (char '#' *> decimal) <*> (string " @ " *> decimal) <*> (char ',' *> decimal) <*> (string ": " *> decimal) <*> (char 'x' *> decimal)

claim :: String -> Claim
claim = either undefined id . parse claimP ""


range :: Int -> Int -> [Int]
range start amount = take amount [start..]

allPairs :: [Int] -> [Int] -> [(Int, Int)]
allPairs xs ys = [(x,y) | x <- xs, y <- ys]

claimInches :: Claim -> [(Int, Int)]
claimInches = allPairs <$> (range <$> x <*> width) <*> (range <$> y <*> height)

idAndInches :: Claim -> [(Int, (Int, Int))]
idAndInches = fmap <$> (,) . claimId <*> claimInches

claimsOnInches :: [String] -> [[(Int, (Int, Int))]]
claimsOnInches = groupSortOn snd . concatMap idAndInches . fmap claim

solve1 :: [String] -> Int
solve1 = length . filter ((>=2) . length) . claimsOnInches

-- How many square inches of fabric are within two or more claims?
solution1 = solve1 <$> input
-- 110389


claimIds :: [[(Int, (Int,Int))]] -> [Int]
claimIds = concatMap (fmap fst)

claimIdsOfInchesWithClaims :: (Int -> Bool) -> [String] -> [Int]
claimIdsOfInchesWithClaims predicate = claimIds . filter (predicate . length) . claimsOnInches

solve2 :: [String] -> Int
solve2 = fromJust . (find <$> flip notMember . Set.fromList . claimIdsOfInchesWithClaims (> 1) <*> claimIdsOfInchesWithClaims (== 1))

-- What is the ID of the only claim that doesn't overlap?
solution2 = solve2 <$> input
-- 552