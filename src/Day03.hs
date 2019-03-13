module Day03 where

import Text.Parsec (parse,many,many1,optional)
import Text.Parsec.Char (char,space,string,letter,digit)
import Text.Parsec.Combinator (between,sepBy)
import Text.ParserCombinators.Parsec.Number (int)
import Control.Arrow ((&&&))
import Data.List (sort,group,groupBy,sortBy,find)
import Data.List.Extra (groupSortBy)
import qualified Data.Set as Set (fromList)
import Data.Set (notMember)
import Data.Maybe (fromJust)

input = lines <$> readFile "input/input03.txt"

data Claim = Claim {
  _claimId :: Int,
  _x :: Int,
  _y :: Int,
  _width :: Int,
  _height :: Int
} deriving Show

claimP = Claim <$> (char '#' *> int) <*> (string " @ " *> int) <*> (char ',' *> int) <*> (string ": " *> int) <*> (char 'x' *> int)

claim = either undefined id . parse claimP ""

claim2inches (Claim claimId x y width height) = [(claimId,xx,yy) | xx <- [x,x+1..(x + width - 1)], yy <- [y,y+1..(y + height - 1)]]

byInch (_,x1,y1) (_,x2,y2) = compare (x1,y1) (x2,y2)

claimsOnInches = groupSortBy byInch . concatMap claim2inches . fmap claim

solve1 = length . filter ((>=2) . length) . claimsOnInches

claimIds = concatMap (fmap (\(a,_,_) -> a))
claimIdsOfInchesWithClaims predicate = claimIds . filter (predicate . length) . claimsOnInches

solve2 = fromJust . (\(multiples,singletons) -> find (`notMember` multiples) singletons) . (Set.fromList . claimIdsOfInchesWithClaims (> 1) &&& claimIdsOfInchesWithClaims (== 1))

-- How many square inches of fabric are within two or more claims?
solution1 = solve1 <$> input
-- 110389

-- What is the ID of the only claim that doesn't overlap?
solution2 = solve2 <$> input
-- 552