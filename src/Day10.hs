module Day10 where

import Control.Applicative.Combinators (count)
import Control.Arrow                        ((&&&))
import Data.Function                        (on)
import Data.List                            (groupBy, nub, sort,
                                                       sortBy)
import Data.List.Extra                      (groupOn, groupSortBy)
import Data.Maybe (fromJust)
import Data.Tuple                           (swap)
import Data.Tuple.Extra                     (first)
import Text.Megaparsec            (Parsec, anySingle, many, optional,
                                             parseMaybe, try, (<|>))
import Text.Megaparsec.Char       (char, letterChar, space, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)


input = lines <$> readFile "input/input10.txt"

data Point = Point {
    position :: (Int,Int),
    velocity :: (Int,Int)
} deriving Show

instance Semigroup Point where
    Point (x,y) _ <> Point _ (vx,vy) = Point (x+vx,y+vy) (vx,vy)

type Parser = Parsec () String

coordinateP = (,) <$> (many (char ' ') *> signed space decimal <* char ',') <*> (many (char ' ') *> signed space decimal)

pointP :: Parser Point
pointP = Point <$> (string "position=<" *> coordinateP) <*> (string "> velocity=<" *> coordinateP) <* char '>'

point = fromJust . parseMaybe pointP

horizontalVariance = length . nub . sort . fmap (fst . position)

groupByY = groupOn (snd . position) . sortBy (compare `on` (snd . position))

minMaxX = (minimum &&& maximum) . fmap (fst . position)

leftPad  minx x = replicate (minx*(-1) + x) ' '
rightPad maxx x = replicate (abs maxx - x) ' '

renderPoint (minx,maxx) (Point (x,_) _) = leftPad minx x <> "#" <> rightPad maxx x

combinePoint '#' _ = '#'
combinePoint _ '#' = '#'
combinePoint _ _   = ' '

combineRow = fmap (uncurry combinePoint) . uncurry zip

renderRow minmax = fmap (foldl1 (curry combineRow) . fmap (renderPoint minmax))

render = uncurry renderRow . (minMaxX &&& groupByY)

solve1 = fmap (id &&& horizontalVariance) . iterate (fmap (\p -> p <> p)) . fmap point

-- What message will eventually appear in the sky?
solution1 = render . fst . head . dropWhile ((> 50) . snd) . solve1 <$> input
-- LXJFKAXA

-- exactly how many seconds would they have needed to wait for that message to appear?
solution2 = fst . head . dropWhile ((> 50) . snd . snd) . zip [0..] . solve1 <$> input
-- 10312