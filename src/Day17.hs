{-# LANGUAGE TupleSections #-}
module Day17 where

import           Control.Arrow ((&&&))
import           Data.Maybe (fromJust, isJust)
import           Text.Parsec (parse,many,many1,optionMaybe,(<|>))
import           Text.Parsec.Char (char,space,string,letter,digit,anyChar)
import           Text.Parsec.Combinator (between,sepBy)
import           Text.ParserCombinators.Parsec.Number (int)
import Data.List (sortOn,concatMap,nub,find)
import qualified Data.Sequence as S
import Data.Sequence (Seq,(<|),(|>))
import Data.Foldable (toList)
import Data.Bifunctor (first,second)
import Data.Maybe (isNothing)

input = lines <$> readFile  "input/input17.txt"

coordOrRangeP = (\c start mend -> (c, maybe [start] (\e -> [start..e]) mend)) <$> (char 'x' <|> char 'y') <*> (char '=' *> int) <*> optionMaybe (string ".." *> int)

rowP = (\[xs,ys] -> [(Clay,x,y) | x <- xs, y <- ys]) . fmap snd . sortOn fst <$> coordOrRangeP `sepBy` string ", "

data Substance = Spring | Empty | Clay | Water | Retained deriving Eq
instance Show Substance where
  show Spring = "+"
  show Empty = "."
  show Clay = "#"
  show Water = "|"
  show Retained = "~"

type Coord = (Substance,Int,Int)
type Map = Seq (Seq Substance)

showMap :: Map -> [String]
showMap = toList . fmap (concatMap show)

clay = either undefined id . parse rowP ""

mkMap :: [Coord] -> Map
mkMap clay = S.fromFunction (1 + maximum (fmap (\(_,_,b) -> b) clay)) (\y -> (Empty <| (S.fromFunction (1 + minimum (fmap (\(_,a,_) -> a) clay) + maximum (fmap (\(_,a,_) -> a) clay)) $ \x -> case find (\(_,a,b) -> a == x && b == y) clay of
  Just (s,_,_) -> s
  Nothing      -> Empty)) |> Empty)

down (x,y) = (x,y+1)
left (x,y) = (x-1,y)
right (x,y) = (x+1,y)

height = length
width = S.length . flip S.index 0

(!) map (x,y) = ((`S.index ` x) . (`S.index` y)) map

set v (x,y) = S.adjust' (S.update x v) y

isBetween _ Nothing _ = False
isBetween _ _ Nothing = False
isBetween x (Just a) (Just b) = a <= x && x <= b

wetLeft  c map = map ! c == Clay || (map ! c `elem` [Water,Retained] && fst c > 0 && wetLeft (left c) map)
wetRight c map = map ! c == Clay || (map ! c `elem` [Water,Retained] && fst c < (width map - 1) && wetRight (right c) map)

retainLeft c map | map ! c == Clay = map
retainLeft c map                   = retainLeft (left c) $ set Retained c map

retainRight c map | map ! c == Clay = map
retainRight c map                   = retainRight (right c) $ set Retained c map

watery :: (Int,Int) -> Map -> Map
watery c@(_,y) map | y < 0 || y >= height map = map
watery c@(_,y) map | y < 0 || y == height map - 1 = set Water c map
watery c@(x,_) map | x < 0 || x >= width  map = map
watery c       map | map ! c == Spring       = watery (down c) map
watery c       map | map ! c == Clay         = map
watery c       map | map ! c == Water        = map
watery c       map | map ! c == Retained     = map
watery c       map                           = let
  foo  = map
  bar  = if snd (down c) < height foo && foo ! down c == Empty then watery (down c) foo else foo
  baz  = if snd (down c) < height bar && bar ! down c `elem` [Water,Retained,Clay] then set Water c bar else bar
  quux =  if snd (down c) < height foo && (baz ! down c == Clay || (wetLeft (down c) baz && wetRight (down c) baz))
          then watery (left c) $ watery (right c) baz
          else baz
 in
  if wetLeft c quux && wetRight c quux then retainLeft c (retainRight c quux) else quux

stripLeft = fmap . first <$> flip (-) . minimum . fmap (\(_,x,_) -> x) <*> id

findSpring = (,0) . fromJust . S.findIndexL (== Spring) . (`S.index` 0)

process = uncurry watery . (findSpring &&& id) . mkMap . stripLeft . ((Spring,500,0) :) . concatMap clay

solve = fmap snd . S.dropWhileL (isNothing . fst) . fmap (S.findIndexL (== Clay) &&& id) . process

solve1 = sum . fmap (length . S.findIndicesL (`elem` [Water,Retained])) . solve

-- How many tiles can the water reach
solution1 = solve1 <$> input
-- 39877

solve2 = sum . fmap (length . S.findIndicesL (== Retained)) . solve

-- How many water tiles are left
solution2 = solve2 <$> input
-- 33291

debug = showMap . process <$> input >>= mapM putStrLn