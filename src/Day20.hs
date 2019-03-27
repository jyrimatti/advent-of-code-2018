{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
module Day20 where

import           Algorithm.Search (bfs,aStar)
import           Control.Applicative (liftA3, many)
import           Control.Arrow ((&&&), second)
import           Control.Monad.Combinators (between,sepBy)

import           Data.Bifunctor (bimap)
import           Data.Foldable (toList)
import           Data.List (intercalate, intersperse, iterate',maximumBy)
import qualified Data.Matrix as M
import           Data.Matrix.Unboxed (Matrix, (!), rows, cols)
import           Data.Maybe (catMaybes, fromJust)
import           Data.Ord (comparing)
import qualified Data.Sequence as S
import           Data.Sequence (Seq,(><),(|>),(<|))
import           Data.Tuple.Extra (both,fst3,thd3)
import qualified Data.Vector.Unboxed as V
import           Data.Zip (zipWith3, zipWith)
import           Prelude hiding (zipWith,zipWith3)
import           Text.Megaparsec (Parsec,parse,optional,(<|>),try)
import           Text.Megaparsec.Char (char,space,string,anyChar)

input = readFile  "input/input20.txt"
test1 = readFile  "input/input20_test1.txt"
test2 = readFile  "input/input20_test2.txt"
test3 = readFile  "input/input20_test3.txt"

type Parser = Parsec () String

type Regex = [Part]
type Branch = [Part]
type Coord = (Int,Int)
data Part = Direction Coord | Br [Branch] | Empty deriving Eq
instance Show Part where
  show (Direction ( 0,-1)) = "W"
  show (Direction (-1, 0)) = "N"
  show (Direction ( 0, 1)) = "E"
  show (Direction ( 1, 0)) = "S"
  show (Br bs) = "(" <> intercalate "|" (fmap (concatMap show) bs) <> ")"
  show Empty = ""

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

height = S.length
width  = S.length . (`S.index` 0)

widen x = (x <|) . (x <|) . (|> x) . (|> x)
newrow = (`S.replicate` '#') . S.length . (`S.index` 0)

extendMap :: Coord -> MapBuilder -> MapBuilder
extendMap (r,c) map | r > 4 && r < height map - 5 && c > 4 && c < width map - 5 = map
extendMap     _ map = fmap (widen '#') . (widen <$> newrow <*> id) $ map

mark 0 _ = '|'
mark _ 0 = '-'

middle = (`div` 2) . width

extendIfNeeded r c dr dc = extendMap <$> bimap (r + dr*2 +) (c + dc*2 +) . (middle &&& middle) <*> id

room r c dr dc = S.adjust' <$> (`S.update` '.'       ) . (+ r) . (+ dr*2) . middle <*> (+ c) . (+ dc*2) . middle <*> id
door r c dr dc = S.adjust' <$> (`S.update` mark dr dc) . (+ r) . (+ dr  ) . middle <*> (+ c) . (+ dc  ) . middle <*> id

extend :: Part -> Coord -> MapBuilder -> (Coord,MapBuilder)
extend (Direction (dr,dc)) (r,c) = ((r + dr*2,c + dc*2),) . door r c dr dc . room r c dr dc . extendIfNeeded r c dr dc
extend (Br (b:bs))             c = (extend (Br bs) <$> const c <*> thd3) . head . dropWhile (not . null . fst3) . iterate' (uncurry3 extendBranch) . (b,c,)
extend (Br [])                 c = (c,)
extend Empty                   c = (c,)


-- Solving

type Map = Matrix Char

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f ~(a,b,c) = f a b c

quux :: (a -> b -> c) -> (a1 -> a) -> (b1 -> b) -> a1 -> b1 -> c
quux f a b = curry (uncurry f . bimap a b)

showMap :: Map -> [String]
showMap = M.toLists

nextLocs = [(-1,0),(0,-1),(0,1),(1,0)]

inside (r,c) map (dr,dc) = r+dr >= 0 && r+dr < rows map && c+dc >= 0 && c+dc < cols map

isDoor (r,c) map (dr,dc) = map ! (r + dr, c + dc) `elem` ['-','|']

nextStates :: Coord -> Map -> [Coord]
nextStates coord@(r,c) = fmap (bimap (+r) (+c) . both (*2)) . (`filter` nextLocs) . (\map -> (&&) <$> isDoor coord map <*> inside coord map)

pathToTarget :: Map -> Coord -> Coord -> Maybe [Coord]
pathToTarget m i t@(tr,tc) = snd <$> aStar (`nextStates` m) (\(r,c) (a,b) -> 1) (\(r,c) -> abs (r-tr) + abs(c-tc)) (== t) i

findCoords :: Char -> Map -> [Coord]
findCoords ch = filter (not . null) . concatMap (\(r,cs) -> fmap (r,) cs) . zip [0..] . fmap (V.toList . V.findIndices (== ch)) . M.toRows

extendBranch []     c = ([],c,)
extendBranch (p:ps) c = uncurry (ps,,) . extend p c

initialMap = extendMap (0,0) . extendMap (0,0) . S.fromList . fmap S.fromList $ [['X']]

buildMap = thd3 . head . dropWhile (not . null . fst3) . iterate' (uncurry3 extendBranch) . (, (0,0), initialMap)

toMap = M.fromLists . toList . fmap toList . buildMap . regex 


shortestPaths :: Map -> [(Coord,Maybe [Coord])]
shortestPaths = fmap <$> ( (\m i t -> (t,pathToTarget m i t)) <$> id <*> head . findCoords 'X') <*> findCoords '.'


solve = fmap (fromJust . snd) . shortestPaths . toMap

-- What is the largest number of doors you would be required to pass through to reach a room?
-- How many rooms have a shortest path from your current location that pass through at least 1000 doors?
solution = (maximum &&& length . filter (>= 1000)) . fmap length . solve <$> input
-- (4239,8205)