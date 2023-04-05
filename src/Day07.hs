{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
module Day07 where

import           Control.Arrow ((&&&))
import           Control.Conditional (if')
import           Data.List (delete, find, iterate', nub, sort)
import           Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import           Data.Tuple.Extra (fst3, snd3, thd3, both)
import           Numeric.Natural ()
import           Text.Megaparsec (Parsec, anySingle, many, optional, parseMaybe, try, (<|>))
import           Text.Megaparsec.Char (char, letterChar, space, string)
import           Text.Megaparsec.Char.Lexer (decimal, signed)
import           Universum.VarArg ((...))
import           Util ((<$$>>), (<$$>>>), (<&>>), (<*<), arg2, const2, (<&), (&>))



input :: IO [String]
input = lines <$> readFile "input/input07.txt"

type Parser = Parsec () String

type Rule = (Char, Char, Maybe Int)

stepP :: Parser Rule
stepP = (,,Nothing) <$> (string "Step " *> anySingle) <*>
                        (string " must be finished before step " *> anySingle <* string " can begin.")

step :: String -> Rule
step = fromJust . parseMaybe stepP

deleteAll :: [Char] -> [Char] -> [Char]
deleteAll = filter . flip notElem

findInitials :: [Rule] -> [Char]
findInitials = deleteAll <$> fmap snd3 <*> fmap fst3

fstOfJusted :: [Rule] -> [Char]
fstOfJusted = fmap fst3 . filter (isJust . thd3)

foo :: [Rule] -> [Char] -> [Char]
foo = fstOfJusted &> (<>) <& sort

nextToSolve :: [Rule] -> [Char]
nextToSolve = nub ... foo <$> id <*> findInitials

delay :: Char -> Int
delay = (+ 59) . length . enumFromTo 'A'

isComplete :: Rule -> Bool
isComplete = (== Just 0) . thd3

currentlyUnderWork :: Int -> [Rule] -> [Char]
currentlyUnderWork = id &> take <& nextToSolve

performWork :: [Char] -> Char -> Maybe Int -> Maybe Int
performWork = if' <$$>>> flip elem
                     <*< Just ... (`maybe` pred) ... delay ... arg2
                     <*< const2 id

determineStatus :: (Char -> Maybe Int -> Maybe Int) -> Rule -> Maybe Int
determineStatus = ($) <$$>>> const
                         <*< fst3 ... arg2
                         <*< thd3 ... arg2

quux :: (Char -> Maybe Int -> Maybe Int) -> Rule -> Rule
quux = (,,) <$$>>> fst3 ... arg2
               <*< snd3 ... arg2
               <*< determineStatus

bar :: Int -> [Rule] -> [Rule]
bar = fmap . quux <$$>> performWork ... currentlyUnderWork <*< arg2

remaining :: Int -> [Rule] -> [Rule]
remaining = id &> bar <& filter (not . isComplete)

complete :: String -> [Rule] -> String
--complete s rs = s <> (nub . fmap fst3 . filter isComplete) rs
--complete = (. nub . fmap fst3 . filter isComplete) . (<>)
--complete = (<>) <&>> id <*< nub . fmap fst3 . filter isComplete
complete = id &> (<>) <& nub . fmap fst3 . filter isComplete

concatFirsTwo :: (b, b, c) -> [b]
concatFirsTwo = (<>) <$> pure . fst3 <*> pure . snd3

removeFrom :: Int -> (String, [Rule]) -> (String, [Rule])
removeFrom = if' <$$>>> ((&&) <$> (== 1) . length <*> isComplete . head) ... snd ... arg2
                    <*< (,[]) ... ((<>) <$> fst <*> concatFirsTwo . head . snd) ... arg2
                    <*< ((,) <$$>> uncurry complete ... arg2 <*< (id &> remaining <& snd))

act :: Int -> [Rule] -> [(String, [Rule])]
act = removeFrom &> iterate' <& ([],)

solve1 :: Int -> [String] -> String
solve1 = fst . fromJust . find (null . snd) ... (id &> act <& fmap step)

-- In what order should the steps in your instructions be completed?
solution1 :: IO String
solution1 = solve1 1 <$> input
-- CFMNLOAHRKPTWBJSYZVGUQXIDE


findFinals :: [Rule] -> [Char]
findFinals = deleteAll <$> fmap fst3 <*> fmap snd3

finalElement :: [Rule] -> [Rule]
finalElement = fmap (,'_',Nothing) . nub . findFinals

withFinalElement :: [Rule] -> [Rule]
withFinalElement = (<>) <$> id <*> finalElement

solve2 :: Int -> [String] -> Int
solve2 = length . tail . takeWhile (not . null . snd) ... (id &> act <& withFinalElement . fmap step)

-- how long will it take to complete all of the steps?
solution2 :: IO Int
solution2 = solve2 5 <$> input
-- 971