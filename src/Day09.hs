module Day09 where

import Prelude hiding (round)
import Text.Parsec (parse,many,many1,optional)
import Text.Parsec.Char (char,space,string,letter,digit,anyChar)
import Text.Parsec.Combinator (between,sepBy)
import Text.ParserCombinators.Parsec.Number (int)
import Data.Maybe (fromJust)
import qualified Data.Sequence as S
import Control.Arrow ((&&&))
import Data.List (iterate')

input = readFile "input/input09.txt"

data Game = Game {
    players :: Int,
    rounds :: Int,

    marbles :: S.Seq Int,
    round :: Int,
    index :: Int,
    player :: Int,
    scores :: S.Seq Int
} deriving Show

gameP = (\p r -> Game p r S.empty 0 0 0 (S.replicate p 0)) <$> (int <* string " players; last marble is worth ") <*> (int <* string " points")

game = either undefined id . parse gameP ""

isSpecial 0     = False
isSpecial round = round `rem` 23 == 0

newIndex (Game _ _ marbles _     _     _ _) | S.length marbles == 0         = 0
newIndex (Game _ _ marbles _     _     _ _) | S.length marbles == 1         = 1
newIndex (Game _ _ marbles round index _ _) | isSpecial round               = (S.length marbles + index - 7) `rem` S.length marbles
newIndex (Game _ _ marbles _     index _ _) | index == S.length marbles - 2 = S.length marbles
newIndex (Game _ _ marbles _     index _ _)                                 = (index + 2) `rem` S.length marbles

getScore = fromJust . uncurry S.lookup . (newIndex &&& marbles)

step g | isSpecial (round g) = g {
  marbles = S.deleteAt (newIndex g) (marbles g),
  round = round g + 1,
  index = newIndex g,
  player = (player g + 1) `rem` players g,
  scores = S.adjust' (\score -> score + round g + getScore g) (player g) (scores g)
}
step g = g {
  marbles = S.insertAt (newIndex g) (round g) (marbles g),
  round   = round g + 1,
  index   = newIndex g,
  player  = (player g + 1) `rem` players g
}

solve1 = maximum . scores . head . dropWhile (\g -> round g <= rounds g) . iterate' step

-- What is the winning Elf's score?
solution1 = solve1 . game <$> input
-- 371284

-- What would the new winning Elf's score be if the number of the last marble were 100 times larger?
solution2 = solve1 . (\g -> g { rounds = rounds g * 100 }) . game <$> input
-- 3038972494