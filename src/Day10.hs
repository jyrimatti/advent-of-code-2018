module Day10 where

import Control.Applicative.Combinators (count)
import Control.Arrow                        ((&&&))
import Control.Conditional (if')
import Data.FoldApp (foldOf)
import Data.Function                        (on)
import Data.List                            (groupBy, nub, sort,
                                                       sortBy, sortOn)
import Data.List.Extra                      (groupOn, groupSortBy)
import Data.Maybe (fromJust)
import Data.Semigroup (Sum(..))
import Data.Tuple                           (swap)
import Data.Tuple.Extra                     (both, first)
import Text.Megaparsec            (Parsec, anySingle, many, optional,
                                             parseMaybe, try, (<|>))
import Text.Megaparsec.Char       (char, letterChar, space, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Universum.VarArg ((...))
import Util ( (<$$>>), (<$$>>>), (<&>>), (<*<), arg2, const2 )

input :: IO [String]
input = lines <$> readFile "input/input10.txt"

type Coordinate = (Int,Int)

data Point = Point {
    position :: Coordinate,
    velocity :: (Int,Int)
} deriving Show

instance Semigroup Point where
    --Point (x,y) _ <> Point _ (vx,vy) = Point (x+vx,y+vy) (vx,vy)
    (<>) = Point <$$>> (both getSum ... (<>) <&>> both Sum . position <*< both Sum . velocity)
                   <*< velocity ... arg2

type Parser = Parsec () String

coordinateP :: Parser Coordinate
coordinateP = (,) <$> (many (char ' ') *> signed space decimal <* char ',')
                  <*> (many (char ' ') *> signed space decimal)

pointP :: Parser Point
pointP = Point <$> (string "position=<" *> coordinateP)
               <*> (string "> velocity=<" *> coordinateP) <* char '>'

point :: String -> Point
point = fromJust . parseMaybe pointP

horizontalVariance :: [Point] -> Int
horizontalVariance = length . nub . sort . fmap (fst . position)

groupByY :: [Point] -> [[Point]]
groupByY = groupOn (snd . position) . sortOn (snd . position)

minMaxX :: [Point] -> (Int, Int)
minMaxX = (minimum &&& maximum) . fmap (fst . position)

leftPad :: Int -> Int -> String
leftPad = ($ ' ') . replicate ... (+) <&>> negate <*< id

rightPad :: Int -> Int -> String
rightPad = ($ ' ') . replicate ... (-) <&>> abs <*< id

renderPoint :: (Int, Int) -> Point -> String
renderPoint = foldOf <$$>>> (leftPad  <&>> fst <*< fst . position)
                        <*< const2 "#"
                        <*< (rightPad <&>> snd <*< fst . position)

combinePoint :: Char -> Char -> Char
--combinePoint '#' _ = '#'
--combinePoint _ '#' = '#'
--combinePoint _ _   = ' '
combinePoint = ($ ' ') . ($ '#') . if' ... (||) `on` (== '#')

combineRow :: String -> String -> String
combineRow = fmap (uncurry combinePoint) ... zip

renderRow :: (Int, Int) -> [[Point]] -> [String]
renderRow = fmap . (foldl1 combineRow .) . fmap . renderPoint

render :: [Point] -> [String]
render = renderRow <$> minMaxX <*> groupByY

solve1 :: [String] -> [([Point], Int)]
solve1 = fmap (id &&& horizontalVariance) . iterate (fmap ((<>) <$> id <*> id)) . fmap point

-- What message will eventually appear in the sky?
solution1 :: IO [String]
solution1 = render . fst . head . dropWhile ((> 50) . snd) . solve1 <$> input
-- LXJFKAXA

-- exactly how many seconds would they have needed to wait for that message to appear?
solution2 :: IO Integer
solution2 = fst . head . dropWhile ((> 50) . snd . snd) . zip [0..] . solve1 <$> input
-- 10312