{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
module Day07 where

import Data.Maybe (fromJust,isNothing,isJust,fromMaybe)
import Data.List (nub, delete, sort)
import Text.Parsec (parse,many,many1,optional)
import Text.Parsec.Char (char,space,string,letter,digit,anyChar)
import Text.Parsec.Combinator (between,sepBy)
import Text.ParserCombinators.Parsec.Number (int)
import Control.Arrow ((&&&))
import Numeric.Natural

input = lines <$> readFile "input/input07.txt"

stepP = (\a b -> (a,b,Nothing)) <$> (string "Step " *> anyChar) <*> (string " must be finished before step " *> anyChar <* string " can begin.")

step = either undefined id . parse stepP ""

_1 (x,_,_) = x
_2 (_,x,_) = x
_3 (_,_,x) = x

deleteAll toRemove = filter (`notElem` toRemove)

nextToSolve :: [(Char,Char,Maybe Int)] -> String
nextToSolve = nub . (\(xs,ys) -> (_1 <$> filter (isJust . _3) xs) <> sort ys) . (id &&& uncurry deleteAll . (fmap _2 &&& fmap _1))

delay :: Char -> Int
delay c = snd $ head $ filter ((== c) . fst) $ zip ['A'..'Z'] [60..] 

isComplete (_,_,Just 0) = True
isComplete (_,_,_)      = False

underWork workers xs a = a `elem` take workers (nextToSolve xs)

performWork True  a = Just . maybe (delay a) (\a -> a-1)
performWork False _ = id

remaining workers = (\xs -> fmap (\(a,b,time) -> (a,b,performWork (underWork workers xs a) a time)) xs) . filter (not . isComplete)

complete completed = (completed <>) . nub . fmap _1 . filter isComplete

removeFrom workers delay (completed,[(a,b,Just 0)]) = (completed <> [a,b]   , [])
removeFrom workers delay (completed, xs           ) = (complete completed xs, remaining workers xs)

solve1 = fst . head . dropWhile (not . null . snd) . iterate (removeFrom 1 $ const 0) . ([],) . fmap step

-- In what order should the steps in your instructions be completed?
solution1 = solve1 <$> input
-- CFMNLOAHRKPTWBJSYZVGUQXIDE

finalElement = fmap (,'_',Nothing) . nub . uncurry deleteAll . (fmap _1 &&& fmap _2)

solve2 workers f = length . tail . takeWhile (not . null . snd) . iterate (removeFrom 5 delay) . (\xs -> ([],xs <> finalElement xs)) . fmap step

-- how long will it take to complete all of the steps?
solution2 = solve2 5 delay <$> input
-- 971