module Day03 where

import Control.Applicative        (liftA2)
import Control.Arrow              ((&&&))
import Data.List                  (find, group, groupBy, sort, sortBy)
import Data.List.Extra            (groupSortOn)
import Data.Maybe                 (fromJust)
import Data.Profunctor
import Data.Set                   as Set (fromList, notMember)
import Data.Tuple.Extra           (snd3, thd3)
import Text.Megaparsec            (Parsec, many, optional, parseMaybe, try,
                                             (<|>))
import Text.Megaparsec.Char       (char, letterChar, space, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Universum ((...))
import Util


input = fmap claim . lines <$> readFile "input/input03.txt"

data Claim = Claim {
  claimId :: Int,
  x       :: Int,
  y       :: Int,
  width   :: Int,
  height  :: Int
} deriving Show

type Parser = Parsec () String

claimP :: Parser Claim
claimP = Claim <$> (char   '#'   *> decimal)
               <*> (string " @ " *> decimal)
               <*> (char   ','   *> decimal)
               <*> (string ": "  *> decimal)
               <*> (char   'x'   *> decimal)

claim :: String -> Claim
claim = fromJust . parseMaybe claimP

range :: Int -- amount
      -> Int -- start
      -> [Int]
-- range = flip (dove take) enumFrom
range = take <&>> id <*< enumFrom

allPairs :: [Int] -> [Int] -> [(Int, Int)]
allPairs = liftA2 (,) -- Like: lift the tuple constructor to work with lists, using the applicative effect (which happens to be cartesian product)
--allPairs = (<*>) . ((,) <$>)
-- allPairs xs ys = (,) <$> xs <*> ys          -- readable version, but pointful :(
-- allPairs xs ys = [(x,y) | x <- xs, y <- ys] -- readable version, but pointful :(

claimInches :: Claim -> [(Int, Int)]
claimInches = allPairs <$> (range <$> width <*> x)
                       <*> (range <$> height <*> y)

type IdsAndInches = [(Int, (Int, Int))]

idAndInches :: Claim -> IdsAndInches
idAndInches = zip <$> repeat . claimId <*> claimInches

claimsOnInches :: [Claim] -> [IdsAndInches]
claimsOnInches = groupSortOn snd . concatMap idAndInches

solve1 :: [Claim] -> Int
solve1 = length . filter ((>=2) . length) . claimsOnInches

-- How many square inches of fabric are within two or more claims?
solution1 = solve1 <$> input
-- 110389


claimIds :: [IdsAndInches] -> [Int]
claimIds = concatMap (fmap fst)

claimIdsOfInchesWithClaims :: (Int -> Bool) -> [IdsAndInches] -> [Int]
--claimIdsOfInchesWithClaims predicate = claimIds . filter (predicate . length)   -- readable, but not point-free
--claimIdsOfInchesWithClaims = (claimIds .) . filter . (. length)                 -- kind of elegant, but difficult to comprehend
--claimIdsOfInchesWithClaims = dimap (. length) (claimIds .) filter               -- profunctor makes this kind of understandable
--claimIdsOfInchesWithClaims =  claimIds ... filter . (. length)                  -- "just transform the 1. arg to filter" but a bit difficult to graps how the data flows
claimIdsOfInchesWithClaims = claimIds ... filter <&>> (. length) <*< id           -- "first arg (predicate) prefixed by length, second passed as-is"
-- Think about the profunctor form like this: We modify the filter function so, that
-- 1) we transform its input (the predicate) with length
-- 2) we transform its output (function from list to list) with claimIds

findNonOverlapping :: [IdsAndInches] -> Maybe Int
findNonOverlapping = find <$> flip notMember . Set.fromList . claimIdsOfInchesWithClaims (> 1)
                          <*> claimIdsOfInchesWithClaims (== 1)

solve2 :: [Claim] -> Int
solve2 = fromJust . findNonOverlapping . claimsOnInches

-- What is the ID of the only claim that doesn't overlap?
solution2 = solve2 <$> input
-- 552