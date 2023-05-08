{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
module Day09 where

import           GHC.Generics (Generic)
import           Control.Conditional (if')
import           Control.Lens (over, set)
import           Data.Generics.Product (getField, field)
import           Data.List (iterate')
import           Data.Maybe (fromJust)
import qualified Data.Sequence as S
import           Text.Megaparsec (Parsec, parseMaybe)
import           Text.Megaparsec.Char (string)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Universum ((...))
import           Util ((<$$>>), (<$$>>>), (<&>>), (<*<), arg2, (<$$$>>), (<$$$>>>))


input :: IO String
input = readFile "input/input09.txt"

data Game = Game {
    currentRound :: Int,
    index        :: Int,
    player       :: Int,
    marbles      :: S.Seq Int,
    
    scores       :: S.Seq Int,

    players      :: Int,
    rounds       :: Int
} deriving (Show, Generic)

gameP :: Parser Game
gameP = (Game 0 0 0 S.empty <$$>>> (`S.replicate` 0) ... const) <*< const <*< arg2 <$> (decimal <* string " players; last marble is worth ")
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
newIndex = if' <$> S.null . marbles
               <*> const 0 <*> (
           if' <$> (== 1) . S.length . marbles
               <*> const 1 <*> (
           if' <$> isSpecial . currentRound
               <*> (rem <$> ((+) <$> S.length . marbles <*> subtract 7 . index) <*> S.length . marbles) <*> (
           if' <$> ( (==) <$> index <*> subtract 2 . S.length . marbles)
               <*> S.length . marbles
               <*> (rem <$> (+2) . index <*> S.length . marbles))))

getScore :: Game -> Int
getScore = (+) <$> currentRound <*> (fromJust ... S.lookup <$> newIndex <*> marbles)

nextPlayer :: Game -> Int
nextPlayer = rem <$> (+1) . player <*> players

updateCurrentPlayerScore :: Game -> S.Seq Int
updateCurrentPlayerScore = S.adjust' <$> (+) . getScore <*> player <*> scores

updateMarbles :: Int -> Game -> S.Seq Int
--updateMarbles newIndex game = S.insertAt newIndex (_round game) (_marbles game)
--updateMarbles = uncurry <&>> S.insertAt <*< (_round &&& _marbles)
updateMarbles = S.insertAt <$$>>> const <*< currentRound ... arg2 
                                        <*< marbles ... arg2

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
-- uuh...
step = over (field @"currentRound") succ .
       (set (field @"player") <$> nextPlayer <*> id) .
       (((.) <$> set (field @"index")
             <*> (if' <$$>>> isSpecial . currentRound ... arg2
                         <*< ((.) <$> over (field @"marbles") . S.deleteAt
                                  <*> (set (field @"scores") <$> updateCurrentPlayerScore <*> id) ... arg2 )
                         <*< (set (field @"marbles") <$$>> updateMarbles <*< arg2)
                 )
        ) <$> newIndex <*> id)

solve1 :: Game -> Int
solve1 = maximum . scores . head . dropWhile ((<=) <$> currentRound <*> rounds) . iterate' step

-- What is the winning Elf's score?
solution1 :: IO Int
solution1 = solve1 . game <$> input
-- 371284

-- What would the new winning Elf's score be if the number of the last marble were 100 times larger?
solution2 :: IO Int
solution2 = solve1 . over (field @"rounds") (*100) . game <$> input
-- 3038972494