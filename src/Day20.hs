{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}
module Day20 where

import           Algorithm.Search (aStar)
import           Control.Arrow ((&&&))
import           Control.Conditional (if')
import           Control.Applicative.Combinators (between, sepBy, many, (<|>))
import           Data.Bifunctor (bimap)
import           Data.Composition ((.***), (.*), (.**))
import           Data.Foldable (toList)
import           Data.FoldApp (allOf, sumOf)
import           Data.Function (on)
import           Data.List (iterate')
import qualified Data.Matrix as M
import           Data.Matrix.Unboxed (Matrix, cols, rows, (!), toLists, toRows, fromLists)
import           Data.Maybe (fromJust)
import qualified Data.Sequence as S
import           Data.Sequence (Seq, (<|), (><), (|>))
import           Data.Tuple.Extra (both, fst3, thd3, uncurry3)
import qualified Data.Vector.Unboxed as V
import           Text.Megaparsec (Parsec, parse, try)
import           Text.Megaparsec.Char (char, string)
import           Universum ((...))
import           Util ((<$$$$$>>), (<$$$$$>>>), (<$$$$>>), (<$$$$>>>)
                     , (<$$$$>>>>), (<$$$>>), (<$$$>>>), (<$$$>>>>), (<$$$>>>>>)
                     , (<$$>>), (<$$>>>), (<$$>>>>), (<&>>), (<*<), arg2, arg31
                     , arg32, arg33, arg41, arg42, arg43, arg44, arg51, arg52
                     , arg53, arg54, arg55, compose3, compose4, const2, const4
                     , const5, (<&), (&>))


input, test1, test2, test3 :: IO String
input = readFile  "input/input20.txt"

test1 = readFile  "input/input20_test1.txt"
test2 = readFile  "input/input20_test2.txt"
test3 = readFile  "input/input20_test3.txt"

type Parser = Parsec () String

type Regex = [Part]
type Branch = [Part]
type Coord = (Int,Int)

data Part = Direction {coord :: Coord} | Br {branches :: [Branch]} | Empty
  deriving Eq

-- Parsing

regexP :: Parser Regex
regexP = between (char '^') (char '$') (many partP)

branchP :: Parser [[Part]]
branchP = (many (try partP) <|> ([Empty] <$ string "")) `sepBy` char '|'

partP :: Parser Part
partP = try (Br        <$> between (char '(') (char ')') branchP) <|>
             Direction <$> directionP

directionP :: Parser Coord
directionP = try ((0 ,-1) <$ char 'W') <|>
             try ((-1, 0) <$ char 'N') <|>
             try ((0 , 1) <$ char 'E') <|>
                  (1 , 0) <$ char 'S'

regex :: String -> Regex
regex = either undefined id . parse regexP ""


-- Map building

type MapBuilder = Seq (Seq Char)

height :: MapBuilder -> Int
height = S.length

width :: MapBuilder -> Int
width  = S.length . (`S.index` 0)

widen :: a -> Seq a -> Seq a
widen = compose4 <$> (<|) <*> (<|) <*> flip (|>) <*> flip (|>)

newrow :: MapBuilder -> Seq Char
newrow = (`S.replicate` '#') . S.length . (`S.index` 0)

extendMap :: Coord -> MapBuilder -> MapBuilder
extendMap = if' @Bool <$$>>> (allOf <$$>>>> (>4) . fst ... const
                                        <*< fst &> (<) <& subtract 5 . height
                                        <*< (>4) . snd ... const
                                        <*< snd &> (<) <& subtract 5 . width)
                         <*< arg2
                         <*< fmap (widen '#') . (widen <$> newrow <*> id) ... arg2

mark :: Int -> Int -> Char
mark = if' <$$>>> (==0) ... const
              <*< const2 '|'
              <*< const2 '-'

middle :: MapBuilder -> Int
middle = (`div` 2) . width

baz1 :: Int -> Int -> Int -> Int -> Int -> Int
baz1 = sumOf <$$$$$>>> arg51 <*< (*2) ... arg53 <*< arg55

baz2 :: Int -> Int -> Int -> Int -> Int -> Int
baz2 = sumOf <$$$$$>>> arg52 <*< (*2) ... arg54 <*< arg55

extendIfNeeded :: Int -> Int -> Int -> Int -> MapBuilder -> MapBuilder
extendIfNeeded = extendMap <$$$$$>> (bimap <$$$$$>>> const .*** baz1
                                                 <*< const .*** baz2
                                                 <*< (middle &&& middle) ... arg55)
                                <*< arg55

room :: Int -> Int -> Int -> Int -> MapBuilder -> MapBuilder
room = S.adjust' <$$$$$>>> (compose4 <$$$$>>>> const4 (`S.update` '.') <*< (+) ... arg41 <*< (+) . (*2) ... arg43 <*< const4 middle)
                       <*< (compose3 <$$$$>>> (+) ... arg42 <*< (+) . (*2) ... arg44 <*< const4 middle)
                       <*< arg55

door :: Int -> Int -> Int -> Int -> MapBuilder -> Seq (Seq Char)
door = S.adjust' <$$$$$>>> (compose4 <$$$$>>>> (flip S.update ... (mark <$$$$>> arg43 <*< arg44)) <*< (+) ... arg41 <*< (+) ... arg43 <*< const4 middle)
                       <*< (compose3 <$$$$>>> (+) ... arg42 <*< (+) ... arg44 <*< const4 middle)
                       <*< arg55

isBr :: Part -> Bool
--isBr (Direction _) = False
--isBr (Br _)        = True
--isBr Empty         = False
isBr = if' <$> (== Empty) <*> const False <*> (
       if' <$> (`elem` [Direction (0,-1), Direction (-1, 0), Direction (0,1), Direction (1,0)]) <*> const False <*>
               const True)

extend :: Part -> Coord -> MapBuilder -> (Coord,MapBuilder)
--extend (Direction (dr,dc)) (r,c) = ((r + dr*2,c + dc*2),) . door r c dr dc . room r c dr dc . extendIfNeeded r c dr dc
--extend (Br (b:bs))             c = (extend (Br bs) <$> const c <*> thd3) . head . dropWhile (not . null . fst3) . iterate' (uncurry3 extendBranch) . (b,c,)
--extend (Br [])                 c = (c,)
--extend Empty                   c = (c,)
extend = if' <$$>>> (== Empty) ... const
                <*< (,) ... arg2 $
         if' <$$>>> (== Br []) ... const
                <*< (,) ... arg2 $
         if' <$$>>> isBr ... const
                <*< ((.) <$$>> (Br . tail . branches &> extend <& id)
                           <*< thd3 . head . dropWhile (not . null . fst3) . iterate' (uncurry3 extendBranch) .** (head . branches &> (,,) <& id) )
                <*< ((compose4 <$$$$>>>> ((,) ... (,) <$$$$>> ((+) <$$$$>> arg41 <*< (*2) ... arg43)
                                                          <*< ((+) <$$$$>> arg42 <*< (*2) ... arg44))
                                     <*< door
                                     <*< room
                                     <*< extendIfNeeded) <$$>>>> fst ... arg2
                                                             <*< snd ... arg2
                                                             <*< fst . coord ... const
                                                             <*< snd . coord ... const)

-- Solving

type Map = Matrix Char

showMap :: Map -> [String]
showMap = toLists

nextLocs :: [(Int, Int)]
nextLocs = [(-1,0),(0,-1),(0,1),(1,0)]

xdx :: (Int, b) -> p2 -> (Int, b) -> Int
xdx = ((+) `on` fst) <$$$>> arg31 <*< arg33

ydy :: (a, Int) -> p2 -> (a, Int) -> Int
ydy = ((+) `on` snd) <$$$>> arg31 <*< arg33

inside :: (Int, Int) -> Map -> (Int, Int) -> Bool
inside = allOf <$$$>>>> (>=0) ... xdx
                    <*< ((<) <$$$>> xdx <*< rows ... arg32)
                    <*< (>=0) ... ydy
                    <*< ((<) <$$$>> ydy <*< cols ... arg32)

isDoor :: (Int, Int) -> Map -> (Int, Int) -> Bool
isDoor = (`elem` ['-','|']) ... ( (.* (,)) . (!) <$$$>>> arg32
                                                      <*< xdx
                                                      <*< ydy)

nextStates :: Coord -> Map -> [Coord]
nextStates = ($) <$$>> fmap . ( (. both (*2)) .* bimap <$> (+) . fst <*> (+) . snd) ... const
                   <*< (`filter` nextLocs) .* ((&&) <$$$>> isDoor <*< inside)

pathToTarget :: Map -> Coord -> Coord -> Maybe [Coord]
pathToTarget = fmap snd ... aStar <$$$>>>>> flip nextStates ... arg31
                                        <*< const5 1
                                        <*< const (const quux)
                                        <*< (==) ... arg33
                                        <*< arg32

quux :: (Int, Int) -> (Int, Int) -> Int
quux = (+) <$$>> abs ... (subtract `on` fst)
             <*< abs ... (subtract `on` snd)

findCoords :: Char -> Map -> [Coord]
findCoords = filter (not . null) . concatMap (fmap <$> (,) . fst <*> snd) . zip [0..] ... (V.toList ... V.findIndices . (==) &> fmap <& toRows)

extendBranch :: [Part] -> (Int, Int) -> MapBuilder -> ([Part], (Int, Int), MapBuilder)
extendBranch = if' <$$>>> null ... const
                      <*< (,,)
                      <*< ((.) <$$>> uncurry . (,,) . tail ... const
                                 <*< head &> extend <& id)

initialMap :: MapBuilder
initialMap = extendMap (0,0) . extendMap (0,0) . S.fromList . fmap S.fromList $ [['X']]

buildMap :: [Part] -> MapBuilder
buildMap = thd3 . head . dropWhile (not . null . fst3) . iterate' (uncurry3 extendBranch) . (, (0,0), initialMap)

toMap :: String -> Map
toMap = fromLists . toList . fmap toList . buildMap . regex 


shortestPaths :: Map -> [(Coord,Maybe [Coord])]
shortestPaths = fmap <$> ( ((,) <$$$>> arg33 <*< pathToTarget) <$> id <*> head . findCoords 'X' )
                     <*> findCoords '.'


solve :: String -> [[Coord]]
solve = fmap (fromJust . snd) . shortestPaths . toMap

-- What is the largest number of doors you would be required to pass through to reach a room?
-- How many rooms have a shortest path from your current location that pass through at least 1000 doors?
solution :: IO (Int, Int)
solution = (maximum &&& length . filter (>= 1000)) . fmap length . solve <$> input
-- (4239,8205)