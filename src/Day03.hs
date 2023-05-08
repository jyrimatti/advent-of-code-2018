module Day03 where

import           Control.Applicative (liftA2)
import           Data.Aviary.Birds (dove, goldfinch)
import           Data.Foldable (find)
import           Data.List.Extra (groupSortOn)
import           Data.Maybe (fromJust)
import           Data.Set as Set (fromList)
import           Text.Megaparsec (Parsec, parseMaybe)
import           Text.Megaparsec.Char (char, string)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Universum ((...))
import           Util ((&>), (<&), (<&>>), (<*<))


input :: IO [Claim]
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
-- applicative parsing is quite naturally point-free

claim :: String -> Claim
claim = fromJust . parseMaybe claimP

range :: Int -- amount
      -> Int -- start
      -> [Int]
--range amount start = take amount [start..]    -- Pointfull.
--range = (. enumFrom) . take                   -- What pointfree.io returns.
--range = flip (dove take) enumFrom             -- Using a Bird.
--range = goldfinch take enumFrom               -- Or another Bird
--range = take <&>> id <*< enumFrom             -- With a general "pass each arg through a separate function"
range = id &> take <& enumFrom                  -- Or the same with a nicer syntax

allPairs :: [Int] -> [Int] -> [(Int, Int)]
-- allPairs xs ys = (,) <$> xs <*> ys          -- readable version, but pointful :(
-- allPairs xs ys = [(x,y) | x <- xs, y <- ys] -- readable version, but pointful :(
-- allPairs = (<*>) . ((,) <$>)
allPairs = liftA2 (,) -- Like: lift the tuple constructor to work with lists, using the applicative effect (which happens to be cartesian product)

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
solution1 :: IO Int
solution1 = solve1 <$> input
-- 110389


claimIds :: [IdsAndInches] -> [Int]
claimIds = concatMap (fmap fst)

claimIdsOfInchesWithClaims :: (Int -> Bool) -> [IdsAndInches] -> [Int]
--claimIdsOfInchesWithClaims predicate = claimIds . filter (predicate . length)   -- readable, but pointful
--claimIdsOfInchesWithClaims = (claimIds .) . filter . (. length)                 -- kind of elegant, but difficult to comprehend
--claimIdsOfInchesWithClaims =  claimIds ... filter . (. length)                  -- "just transform the 1. arg to filter" but a bit difficult to graps how the data flows
--claimIdsOfInchesWithClaims = dimap (. length) (claimIds .) filter               -- profunctor makes this kind of thing...
--claimIdsOfInchesWithClaims = claimIds ... lmap (. length) filter                -- ...understandable!
claimIdsOfInchesWithClaims = (. length) &> claimIds ... filter <& id              -- "first arg (predicate) prefixed by length, second passed as-is"

findNonOverlapping :: [IdsAndInches] -> Maybe Int
findNonOverlapping = find <$> flip notElem . Set.fromList . claimIdsOfInchesWithClaims (> 1)
                          <*> claimIdsOfInchesWithClaims (== 1)

solve2 :: [Claim] -> Int
solve2 = fromJust . findNonOverlapping . claimsOnInches

-- What is the ID of the only claim that doesn't overlap?
solution2 :: IO Int
solution2 = solve2 <$> input
-- 552