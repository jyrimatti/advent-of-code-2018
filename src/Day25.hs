module Day25 where

import           Algorithm.Search           (bfs)
import           Control.Arrow              ((&&&))
import           Control.Monad              ((>=>))
import           Control.Monad.Combinators  (between, manyTill, sepBy)
import           Data.Bifunctor             (bimap, first, second)
import           Data.Foldable              (foldl', maximumBy, toList)
import           Data.Function              (on)
import           Data.List                  (cycle, delete, group, groupBy,
                                             iterate', nub, nubBy, sort, sortBy,
                                             sortOn, zip, (\\))
import           Data.List.Extra            (groupSortBy)
import qualified Data.Matrix                as M
import           Data.Matrix                (Matrix, (!))
import           Data.Maybe                 (catMaybes, fromJust, fromMaybe,
                                             isJust, isNothing, listToMaybe,
                                             mapMaybe, maybeToList)
import           Data.Ord                   (Down (..), comparing)
import qualified Data.Sequence              as S
import           Data.Sequence              (Seq (..))
import           Data.Tuple                 (swap)
import           Data.Tuple.Extra           (both, swap)
import qualified Data.Vector                as V
import           Data.Vector                (Vector)
import Debug.Trace ()
import           Prelude                    hiding (Either (Left, Right), round)
import           Text.Megaparsec            (Parsec, anySingle, many, optional,
                                             parse, try, (<|>))
import           Text.Megaparsec.Char       (char, letterChar, space, string)
import           Text.Megaparsec.Char.Lexer (decimal, signed)


inputLines :: FilePath -> IO [String]
inputLines = fmap lines . readFile 

input :: IO [String]
input = inputLines "input/input25.txt"

test1, test2, test3, test4 :: IO [String]
test1  = inputLines "input/input25_test1.txt"
test2  = inputLines "input/input25_test2.txt"
test3  = inputLines "input/input25_test3.txt"
test4  = inputLines "input/input25_test4.txt"

type Parser = Parsec () String

type Point = (Int,Int,Int,Int)

pointP :: Parser Point
pointP = (\[a,b,c,d] -> (a,b,c,d)) <$> (space *> signed space decimal `sepBy` char ',')

point :: String -> Point
point = either undefined id . parse pointP ""

manhattan :: Num a => (a, a, a, a) -> (a, a, a, a) -> a
manhattan (a1,b1,c1,d1) (a2,b2,c2,d2) = abs (a1-a2) + abs (b1-b2) + abs (c1-c2) + abs (d1-d2)

join :: Seq [Point] -> Point -> Seq [Point]
join constellations point = case S.findIndexL (any $ (<= 3) . manhattan point) constellations of
    Just i -> S.adjust' (point :) i constellations
    Nothing -> constellations :|> [point]

collapse :: [Point] -> [Point] -> Bool
collapse as bs | as == bs = False 
collapse as bs = any (<= 3) $ concatMap (\a -> fmap (($ a) . manhattan) bs) as

foo :: Seq [Point] -> Seq [Point]
foo cs = let
    mi = S.findIndexL (\c -> or $ fmap (($ c) . collapse) cs) cs
  in
    case mi of
        Just i -> let
            constellationToRemove = S.index cs i
            constellationToMergeTo = fromJust $ S.findIndexL (collapse constellationToRemove) cs
          in
            S.deleteAt i $ S.adjust' (constellationToRemove <>) constellationToMergeTo cs
        Nothing -> cs

skipWhileAdvancing :: [Seq [Point]] -> [Seq [Point]]
skipWhileAdvancing = fmap snd . dropWhile (uncurry (/=)) . (zip <$> id <*> tail)

solve1 :: [String] -> Seq [Point]
solve1 = head . skipWhileAdvancing . iterate' foo . foldl' join S.empty . fmap point

-- How many constellations are formed by the fixed points in spacetime?
solution1 :: IO Int
solution1 = length . solve1 <$> input
-- 375