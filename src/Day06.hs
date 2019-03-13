module Day06 where

import Data.List (minimumBy,maximumBy,sort,group,nub,find,null)
import Data.Function (on)
import Data.Bifunctor (bimap)
import Control.Arrow ((&&&))
import Text.Parsec (parse,many,many1,optional)
import Text.Parsec.Char (char,space,string,letter,digit)
import Text.Parsec.Combinator (between,sepBy)
import Text.ParserCombinators.Parsec.Number (int)
import Data.Tuple.Extra (first,second,both)

input = lines <$> readFile "input/input06.txt"

coordinateP = (,) <$> (int <* string ", ") <*> int

coordinate = either undefined id . parse coordinateP ""

distance (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

relevantWorld cs = ((minimum $ fmap fst cs, minimum $ fmap snd cs),(maximum $ fmap fst cs, maximum $ fmap snd cs))

allRelevantCoordinates ((x1,y1),(x2,y2)) = [(x,y,x==x1||x==x2||y==y1||y==y2)|x <- [x1..x2], y <- [y1..y2]]

pairwiseDistancesTo coords = fmap (\(w1,w2,isBordering) -> (fmap (distance (w1,w2)) coords, isBordering))

hasDuplicateMinimums = not . null . concatMap nub . find ((> 1) . length) . take 1 . group . sort

removeElements xs = filter ((`notElem` xs) . fst)

havingExactMinimum = not . hasDuplicateMinimums . fst

indexOfMinimum = fst . minimumBy (compare `on` snd) . zip [0..]

gatherInfinite = nub . fmap fst . filter snd

mostFrequentValue = maximum . fmap length . group . sort

spread f arg1 arg2 = uncurry f . (arg1 &&& arg2)

distances = spread pairwiseDistancesTo id (allRelevantCoordinates . relevantWorld) . fmap coordinate

solve1 = mostFrequentValue . spread removeElements gatherInfinite id . fmap (first indexOfMinimum) . filter havingExactMinimum . distances

solve2 = length . filter (<10000) . fmap (sum . fst) . distances

-- What is the size of the largest area that isn't infinite?
solution1 = solve1 <$> input
-- 5035

-- What is the size of the region containing all locations which have a total distance to all given coordinates of less than 10000?
solution2 = solve2 <$> input
-- 35294