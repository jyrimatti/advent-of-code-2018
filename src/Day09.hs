{-# LANGUAGE TemplateHaskell #-}
module Day09 where

import           Control.Applicative.Combinators (count)
import           Control.Arrow                        ((&&&))
import           Control.Conditional (if')
import           Control.Lens hiding ((...),index)
import           Data.List                            (iterate')
import           Data.Maybe (fromJust)
import qualified Data.Sequence                        as S
import           Data.Tuple.Extra (uncurry3)
import           Prelude                              hiding (round)
import           Text.Megaparsec            (Parsec, anySingle, many, optional,
                                             parseMaybe, try, (<|>))
import           Text.Megaparsec.Char       (char, letterChar, space, string)
import           Text.Megaparsec.Char.Lexer (decimal, signed)
import           Universum.VarArg ((...))
import           Util

input = readFile "input/input09.txt"

data Game = Game {
    _players :: Int,
    _rounds  :: Int,

    _marbles :: S.Seq Int,
    _round   :: Int,
    _index   :: Int,
    _player  :: Int,
    _scores  :: S.Seq Int
} deriving Show

makeLenses ''Game

gameP :: Parser Game
gameP = ( ($ 0) . ($ 0) . ($ 0) . ($ S.empty) <$$>> Game <*< flip S.replicate 0 ... arg1 ) <$> (decimal <* string " players; last marble is worth ")
                                                                                           <*> (decimal <* string " points")

type Parser = Parsec () String

game :: String -> Game
game = fromJust . parseMaybe gameP

isSpecial :: Int -> Bool
isSpecial = (&&) <$> (/= 0) <*> (== 0) . (`rem` 23)

newIndex :: Game -> Int
{-
newIndex (Game _ _ marbles _     _     _ _) | S.length marbles == 0         = 0
newIndex (Game _ _ marbles _     _     _ _) | S.length marbles == 1         = 1
newIndex (Game _ _ marbles round index _ _) | isSpecial round               = (S.length marbles + index - 7) `rem` S.length marbles
newIndex (Game _ _ marbles _     index _ _) | index == S.length marbles - 2 = S.length marbles
newIndex (Game _ _ marbles _     index _ _)                                 = (index + 2) `rem` S.length marbles
-}
newIndex = if' <$> (== 0) . S.length . _marbles
               <*> const 0 <*> (
           if' <$> (== 1) . S.length . _marbles
               <*> const 1 <*> (
           if' <$> isSpecial . _round
               <*> (rem <$> ((+) <$> S.length . _marbles <*> flip (-) 7 . _index) <*> S.length . _marbles) <*> (
           if' <$> ( (==) <$> _index <*> flip (-) 2 . S.length . _marbles)
               <*> S.length . _marbles
               <*> (rem <$> (+2) . _index <*> S.length . _marbles))))

getScore :: Game -> Int
getScore = (+) <$> _round <*> (fromJust ... S.lookup <$> newIndex <*> _marbles)

nextPlayer :: Game -> Int
nextPlayer = rem <$> (+1) . _player <*> _players

updateCurrentPlayerScore :: Game -> S.Seq Int
updateCurrentPlayerScore = S.adjust' <$> (+) . getScore <*> _player <*> _scores

updateMarbles :: Int {-newIndex-} -> Game -> S.Seq Int
updateMarbles = uncurry <&>> S.insertAt <*< (_round &&& _marbles)

step :: Game -> Game
{-step g | isSpecial (round g) = g {
  marbles = S.deleteAt (newIndex g) (marbles g),
  round = round g + 1,
  index = newIndex g,
  player = nextPlayer g,
  scores = updateCurrentPlayerScore g
}
step g = g {
  marbles = S.insertAt (newIndex g) (round g) (marbles g),
  round   = round g + 1,
  index   = newIndex g,
  player  = nextPlayer g
}-}
step = over round succ .
       (set player <$> nextPlayer <*> id) .
       (((.) <$> set index
             <*> (if' <$$>>> isSpecial . _round ... arg2
                         <*< ((.) <$> over marbles . S.deleteAt
                                  <*> (set scores <$> updateCurrentPlayerScore <*> id) ... arg2 )
                         <*< (set marbles <$$>> updateMarbles <*< arg2)
                 )
        ) <$> newIndex <*> id)

solve1 = maximum . _scores . head . dropWhile ((<=) <$> _round <*> _rounds) . iterate' step

-- What is the winning Elf's score?
solution1 = solve1 . game <$> input
-- 371284

-- What would the new winning Elf's score be if the number of the last marble were 100 times larger?
solution2 = solve1 . over rounds (*100) . game <$> input
-- 3038972494