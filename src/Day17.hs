{-# LANGUAGE TupleSections #-}
module Day17 where

import           Control.Applicative (liftA2)
import           Control.Applicative.Combinators (count, between, sepBy, optional)
import           Control.Arrow                        ((&&&))
import           Control.Conditional (if')
import           Data.Bifunctor                       (first, second, bimap)
import           Data.Foldable                        (toList)
import           Data.FoldApp (allOf, foldOf)
import           Data.List                            (concatMap, find, nub,
                                                       sortOn)
import           Data.Maybe                           (fromJust, isJust,
                                                       isNothing)
import qualified Data.Sequence                        as S
import           Data.Sequence                        (Seq, (<|), (|>))
import           Data.Tuple.Extra (both, fst3, snd3, thd3)
import           Text.Megaparsec            (Parsec, anySingle, many, optional,
                                             parseMaybe, try, (<|>))
import           Text.Megaparsec.Char       (char, letterChar, space, string)
import           Text.Megaparsec.Char.Lexer (decimal, signed)
import           Universum.VarArg ((...))
import           Util

input :: IO [String]
input = lines <$> readFile  "input/input17.txt"

type Parser = Parsec () String

build :: a -> Int -> Maybe Int -> (a, [Int])
build = (,) <$$$>> arg31 <*< argDrop (maybe <$$>>> singleton ... arg1
                                               <*< enumFromTo ... arg1
                                               <*< arg2)

coordOrRangeP :: Parser (Char, [Int])
coordOrRangeP = build <$> (char 'x' <|> char 'y') <*> (char '=' *> decimal) <*> optional (string ".." *> decimal)

rowP :: Parser [Coord]
rowP = (liftA2 (Clay,,) <$> head <*> last) . fmap snd . sortOn fst <$> coordOrRangeP `sepBy` string ", "

data Substance = Spring | Empty | Clay | Water | Retained deriving Eq
instance Show Substance where
  show Spring   = "+"
  show Empty    = "."
  show Clay     = "#"
  show Water    = "|"
  show Retained = "~"

type Coord = (Substance,Int,Int)
type Map = Seq (Seq Substance)

showMap :: Map -> [String]
showMap = toList . fmap (concatMap show)

clay :: String -> [Coord]
clay = fromJust . parseMaybe rowP

foo :: Int -> Int -> [Coord] -> Maybe Coord
foo = find ...$$ ((&&) <$$$>> ((==) <$$$>> arg31 <*< snd3 ... arg33)
                          <*< ((==) <$$$>> arg32 <*< thd3 ... arg33))

qux :: Int -> Int -> [Coord] -> Substance
qux = maybe Empty fst3 ... foo

middle :: [Coord] -> Int -> Seq Substance
middle = S.fromFunction <$$>> succ . ((+) <$> minimum <*> maximum) . fmap snd3 ... arg1
                          <*< flip (flip . flip qux) -- (\a b c -> qux c b a)

mkMap :: [Coord] -> Map
mkMap = S.fromFunction <$> succ . maximum . fmap thd3
                       <*> (|> Empty) . (Empty <|) ... middle

down :: (Int, Int) -> (Int, Int)
down = second succ

left :: (Int, Int) -> (Int, Int)
left = first pred

right :: (Int, Int) -> (Int, Int)
right = first succ

height :: Seq a -> Int
height = length

width :: Map -> Int
width = S.length . flip S.index 0

substance :: (Int, Int) -> Map -> Substance
substance = uncurry $ (.) <&>> flip S.index <*< flip S.index

set :: (Int, Int) -> Substance -> Map -> Map
set = S.adjust' <$$>> S.update . fst
                  <*< const . snd

wetLeft :: (Int, Int) -> Map -> Bool
wetLeft = (||) <$$>> (== Clay) ... substance
                 <*< (allOf <$$>>> (`elem` [Water,Retained]) ... substance
                               <*< (>0) . fst ... arg1
                               <*< wetLeft . left)

wetRight :: (Int, Int) -> Map -> Bool
wetRight = (||) <$$>> (== Clay) ... substance
                 <*< (allOf <$$>>> (`elem` [Water,Retained]) ... substance
                               <*< ((<) <&>> fst <*< pred . width)
                               <*< wetRight .right)

retainLeft :: (Int, Int) -> Map -> Map
retainLeft = if' <$$>>> (== Clay) ... substance
                    <*< arg2
                    <*< (retainLeft <$$>> left ... arg1 <*< (`set` Retained))

retainRight :: (Int, Int) -> Map -> Map
retainRight = if' <$$>>> (== Clay) ... substance
                    <*< arg2
                    <*< (retainRight <$$>> right ... arg1 <*< (`set` Retained))

watery :: (Int,Int) -> Map -> Map
watery = if' <$$>>> ((||) <$$>> (<0) . snd ... arg1 <*< ( (>=) <&>> snd <*< height))
                <*< arg2 $
         if' <$$>>> ((||) <$$>> (<0) . snd ... arg1 <*< ( (==) <&>> snd <*< pred . height))
                <*< flip set Water $
         if' <$$>>> ((||) <$$>> (<0) . fst ... arg1 <*< ( (>=) <&>> fst <*< width))
                <*< arg2 $
         if' <$$>>> (== Spring) ... substance
                <*< watery . down $
         if' <$$>>> (`elem` [Clay, Water, Retained]) ... substance
                <*< arg2
                <*< (blah <$$>> arg1 <*< (quux <$$>> arg1 <*< (baz <$$>> arg1 <*< bar)))

blah :: (Int, Int) -> Map -> Map
blah = if' <$$>>> ((&&) <$$>> wetLeft <*< wetRight)
              <*< (retainLeft <$$>> arg1 <*< retainRight)
              <*< arg2

bar :: (Int, Int) -> Map -> Map
bar = if' <$$>>> ((&&) <$$>> verticallyInside
                         <*< (== Empty) ... substance . down)
             <*< watery . down
             <*< arg2

baz :: (Int, Int) -> Map -> Map
baz = if' <$$>>> ((&&) <$$>> verticallyInside
                         <*< ((`elem` [Water,Retained,Clay]) ... substance . down))
             <*< flip set Water
             <*< arg2

quux :: (Int, Int) -> Map -> Map
quux = if' <$$>>> ((&&) <$$>> verticallyInside
                          <*< ( (||) <$$>> (== Clay) ... substance . down
                                       <*< ( (&&) <$$>> wetLeft . down <*< wetRight . down)))
              <*< (watery <$$>> left ... arg1 <*< watery . right)
              <*< arg2

verticallyInside :: (Int, Int) -> Seq a -> Bool
verticallyInside = (<=) <&>> snd <*< height

stripLeft :: [Coord] -> [Coord]
stripLeft = fmap . first <$> flip (-) . minimum . fmap snd3 <*> id

findSpring :: Map -> (Int, Int)
findSpring = (,0) . fromJust . S.findIndexL (== Spring) . (`S.index` 0)

process :: [String] -> Map
process = (watery <$> findSpring <*> id) . mkMap . stripLeft . ((Spring,500,0) :) . concatMap clay

solve :: [String] -> Map
solve = fmap snd . S.dropWhileL (isNothing . fst) . fmap (S.findIndexL (== Clay) &&& id) . process

solve1 :: [String] -> Int
solve1 = sum . fmap (length . S.findIndicesL (`elem` [Water,Retained])) . solve

-- How many tiles can the water reach
solution1 = solve1 <$> input
-- 39877

solve2 = sum . fmap (length . S.findIndicesL (== Retained)) . solve

-- How many water tiles are left
solution2 = solve2 <$> input
-- 33291

debug = showMap . process <$> input >>= mapM putStrLn