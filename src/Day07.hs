{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
module Day07 where

import Control.Arrow              ((&&&))
import Data.List                  (delete, find, iterate', nub, sort)
import Data.Maybe                 (fromJust, fromMaybe, isJust,
                                             isNothing)
import Data.Tuple.Extra           (fst3, snd3, thd3)
import Numeric.Natural
import Text.Megaparsec            (Parsec, anySingle, many, optional,
                                             parse, try, (<|>))
import Text.Megaparsec.Char       (char, letterChar, space, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)


input = lines <$> readFile "input/input07.txt"

type Parser = Parsec () String

type Rule = (Char, Char, Maybe Int)

stepP :: Parser Rule
stepP = (,,Nothing) <$> (string "Step " *> anySingle) <*> (string " must be finished before step " *> anySingle <* string " can begin.")

step :: String -> Rule
step = either undefined id . parse stepP ""

deleteAll :: String -> String -> String
deleteAll toRemove = filter (`notElem` toRemove)

nextToSolve :: [Rule] -> String
nextToSolve = nub . ((\xs -> ((fst3 <$> filter (isJust . thd3) xs) <>) . sort) <$> id <*> (deleteAll <$> fmap snd3 <*> fmap fst3))

delay :: Char -> Int
delay c = snd $ head $ filter ((== c) . fst) $ zip ['A'..'Z'] [60..] 

isComplete :: Rule -> Bool
isComplete = ((&&) <$> isJust <*> (== 0) . fromJust) . thd3

currentlyWorking :: Int -> [Rule] -> String
currentlyWorking workers = take workers . nextToSolve

underWork :: Int -> Char -> [Rule] -> Bool
underWork workers a = (a `elem`) . currentlyWorking workers

performWork :: String -> Char -> Maybe Int -> Maybe Int
performWork cur a | a `elem` cur = Just . maybe (delay a) (\a -> a - 1)
performWork _   _                = id

remaining :: Int -> [Rule] -> [Rule]
remaining workers = ((\f -> fmap (\(a,b,time) -> (a,b,f a time))) <$> performWork . currentlyWorking workers <*> id) . filter (not . isComplete)

complete :: String -> [(Char, Char, Maybe Int)] -> String
complete completed = (completed <>) . nub . fmap fst3 . filter isComplete

removeFrom :: Int -> (String, [Rule]) -> (String, [Rule])
removeFrom workers (completed,[(a,b,Just 0)]) = (completed <> [a,b]   , [])
removeFrom workers (completed, xs           ) = (complete completed xs, remaining workers xs)

act :: Int -> [Rule] -> [(String, [Rule])]
act workers = iterate' (removeFrom workers) . ([],)

solve1 :: Int -> [String] -> String
solve1 workers = fst . fromJust . find (null . snd) . act workers . fmap step

-- In what order should the steps in your instructions be completed?
solution1 = solve1 1 <$> input
-- CFMNLOAHRKPTWBJSYZVGUQXIDE


finalElement :: [Rule] -> [Rule]
finalElement = fmap (,'_',Nothing) . nub . (deleteAll <$> fmap fst3 <*> fmap snd3)

solve2 :: Int -> [String] -> Int
solve2 workers = length . tail . takeWhile (not . null . snd) . act workers . ((<>) <$> id <*> finalElement) . fmap step

-- how long will it take to complete all of the steps?
solution2 = solve2 5 <$> input
-- 971