{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}
module Day20 where

import           Algorithm.Search          (aStar, bfs)
import           Control.Applicative       (liftA3, many)
import           Control.Arrow             (second, (&&&))
import           Control.Conditional (if')
import           Control.Monad.Combinators (between, sepBy)

import           Data.Bifunctor            (bimap)
import           Data.Foldable             (toList)
import           Data.FoldApp (allOf, sumOf)
import           Data.List                 (intercalate, intersperse, iterate',
                                            maximumBy)
import qualified Data.Matrix               as M
import           Data.Matrix.Unboxed       (Matrix, cols, rows, (!), toLists, toRows, fromLists)
import           Data.Maybe                (catMaybes, fromJust)
import           Data.Ord                  (comparing)
import qualified Data.Sequence             as S
import           Data.Sequence             (Seq, (<|), (><), (|>))
import           Data.Tuple.Extra          (both, fst3, thd3, uncurry3)
import qualified Data.Vector.Unboxed       as V
import           Data.Zip                  (zipWith, zipWith3)
import           Prelude                   hiding (zipWith, zipWith3)
import           Text.Megaparsec           (Parsec, anySingle, optional, parse,
                                            try, (<|>))
import           Text.Megaparsec.Char      (char, space, string)
import           Universum.VarArg ((...))
import           Util
import Data.Composition ((.***), (.*))
import Universum (on)

input :: IO String
input = readFile  "input/input20.txt"

test1 :: IO String
test1 = readFile  "input/input20_test1.txt"

test2 :: IO String
test2 = readFile  "input/input20_test2.txt"

test3 :: IO String
test3 = readFile  "input/input20_test3.txt"

type Parser = Parsec () String

type Regex = [Part]
type Branch = [Part]
type Coord = (Int,Int)
data Part = Direction {coord :: Coord} | Br {branches :: [Branch]} | Empty deriving Eq
instance Show Part where
  show (Direction ( 0,-1)) = "W"
  show (Direction (-1, 0)) = "N"
  show (Direction ( 0, 1)) = "E"
  show (Direction ( 1, 0)) = "S"
  show (Br bs)             = "(" <> intercalate "|" (fmap (concatMap show) bs) <> ")"
  show Empty               = ""

-- Parsing

regexP :: Parser Regex
regexP = between (char '^') (char '$') (many partP)

branchP = (many (try partP) <|> ([Empty] <$ string "")) `sepBy` char '|'

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

foo :: (Int, b) -> MapBuilder -> Bool
foo = (<) <&>> fst <*< flip (-) 5 . height

bar :: (a, Int) -> MapBuilder -> Bool
bar = (<) <&>> snd <*< flip (-) 5 . width

extendMap :: Coord -> MapBuilder -> MapBuilder
extendMap = if' @Bool <$$>>> (allOf <$$>>>> (>4) . fst ... const <*< foo <*< (>4) . snd ... const <*< bar)
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

extend :: Part -> Coord -> MapBuilder -> (Coord,MapBuilder)
extend (Direction (dr,dc)) (r,c) = ((r + dr*2,c + dc*2),) . door r c dr dc . room r c dr dc . extendIfNeeded r c dr dc
extend (Br (b:bs))             c = (extend (Br bs) <$> const c <*> thd3) . head . dropWhile (not . null . fst3) . iterate' (uncurry3 extendBranch) . (b,c,)
extend (Br [])                 c = (c,)
extend Empty                   c = (c,)

-- need pattern matching for this...
isBr :: Part -> Bool
isBr (Direction _) = False
isBr (Br _)        = True
isBr Empty         = False

extend2 :: Part -> Coord -> MapBuilder -> (Coord,MapBuilder)
extend2 = if' <$$>>> (== Empty) ... const
                 <*< (,) ... arg2 $
          if' <$$>>> (== Br []) ... const
                 <*< (,) ... arg2 $
          if' <$$>>> isBr ... const
                 <*< ((.) <$$>> (extend <&>> Br . tail . branches <*< id)
                            <*< thd3 . head . dropWhile (not . null . fst3) . iterate' (uncurry3 extendBranch) ... ((,,) <&>> head . branches <*< id) )
                 <*< ((compose4 <$$$$>>>> (baz <$$$$>>>> arg41 <*< (*2) ... arg43 <*< arg42 <*< (*2) ... arg44) <*< door <*< room <*< extendIfNeeded) <$$>>>> fst ... arg2 <*< snd ... arg2 <*< fst . coord ... const <*< snd . coord ... const)

baz :: Int -> Int -> Int -> Int -> a -> ((Int, Int),a)
baz = (,) ... (,) <$$$$>> ((+) <$$$$>> arg41 <*< arg42) <*< ((+) <$$$$>> arg41 <*< arg42)

-- Solving

type Map = Matrix Char

showMap :: Map -> [String]
showMap = toLists

nextLocs :: [(Int, Int)]
nextLocs = [(-1,0),(0,-1),(0,1),(1,0)]

xdx = ((+) `on` fst) <$$$>> arg31 <*< arg33
ydy = ((+) `on` snd) <$$$>> arg31 <*< arg33

inside :: (Int, Int) -> Map -> (Int, Int) -> Bool
inside = allOf <$$$>>>> (>=0) ... xdx
                    <*< ((<) <$$$>> xdx <*< rows ... arg32)
                    <*< (>=0) ... ydy
                    <*< ((<) <$$$>> ydy <*< cols ... arg32)

isDoor :: (Int, Int) -> Map -> (Int, Int) -> Bool
isDoor = (`elem` ['-','|']) ... ( (... (,)) . (!) <$$$>>> arg32
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
quux = (+) <$$>> abs ... (flip (-) `on` fst)
             <*< abs ... (flip (-) `on` snd)

findCoords :: Char -> Map -> [Coord]
findCoords = filter (not . null) . concatMap (fmap <$> (,) . fst <*> snd) . zip [0..] ... fmap <&>> (V.toList ... V.findIndices . (==)) <*< toRows

extendBranch :: [Part] -> (Int, Int) -> MapBuilder -> ([Part], (Int, Int), MapBuilder)
extendBranch = if' <$$>>> null ... const
                      <*< (,,)
                      <*< ((.) <$$>> uncurry . (,,) . tail ... const
                                 <*< (extend <&>> head <*< id))

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
solution = (maximum &&& length . filter (>= 1000)) . fmap length . solve <$> input
-- (4239,8205)