{-# LANGUAGE TemplateHaskell #-}
module Day14 where

import           Control.Conditional (if')
import           Control.Lens hiding ((...),index,(<|),(|>),transform,both,anyOf,allOf)
import           Data.Foldable    (toList)
import           Data.List        (isPrefixOf, isSuffixOf, iterate', tails)
import           Data.Maybe (fromJust, maybeToList)
import qualified Data.Sequence    as S
import           Data.Sequence    ((><), (|>))
import           Data.Tuple.Extra (both)
import           Prelude          hiding ((++))
import           Universum.VarArg ((...))
import           Util

input = 607331

newtype Elf = Elf {
    _current :: Int
} deriving Show

makeLenses  ''Elf

type Recipies = S.Seq Int
(!) = S.index

recipies = S.fromList [3,7]

elves = (Elf 0, Elf 1)

toDigits :: Int -> [Int]
toDigits = fmap (read . singleton) . show

newRecipies :: Recipies -> (Elf,Elf) -> [Int]
--newRecipies recipies (Elf current1,Elf current2) = toDigits ((recipies ! current1) + (recipies ! current2))
newRecipies = toDigits ... (+) <$$>> ((!) <&>> id <*< _current . fst)
                                 <*< ((!) <&>> id <*< _current . snd)

updateRecipie :: Recipies -> Elf -> Elf
--updateRecipie recipies elf@(Elf current) = elf { _current = (current + (recipies ! current) + 1) `mod` length recipies }
updateRecipie = over current <&>> modifier <*< id

modifier :: Recipies -> Int -> Int
modifier = mod <$$>> succ ... ((+) <$$>> arg2 <*< (!))
                 <*< length ... const

updatedRecipies :: Recipies -> (Elf, Elf) -> Recipies
--updatedRecipies recipies elves = case newRecipies recipies elves of
--        [a,b] -> recipies |> a |> b
--        [a]   -> recipies |> a
updatedRecipies = (><) <$$>> const <*< S.fromList ... newRecipies

step :: Recipies -> (Elf,Elf) -> (Recipies,(Elf,Elf))
step = ((,) <$$>> const <*< (both <$$>> updateRecipie ... const <*< arg2)) <$$>> updatedRecipies <*< arg2

foo2 :: Int -> S.Seq Int -> [Int]
foo2 n seq = case S.viewr seq of
    s S.:> r -> r : lastnReversed (n-1) s
    S.EmptyR -> []
-- Not performant enough
--foo2 = if' <$$>>> null ... arg2
--              <*< const2 []
--              <*< ((:) <$$>> fromJust . (S.lookup <$> pred . length <*> id) ... arg2
--                         <*< (lastnReversed <&>> pred <*< id))

lastnReversed :: Int -> Recipies -> [Int]
lastnReversed = if' <$$>>> (== 0) ... const
                       <*< const2 [] $
                if' <$$>>> (== 1) ... const
                       <*< maybeToList . (S.lookup <$> pred . length <*> id) ... arg2
                       <*< foo2

bar :: Int -> [(S.Seq a, (b, b1))] -> S.Seq a
bar = fst . fmap fst . head ... dropWhile . (. length . fst) . (>) . (+10)

steps = iterate' (uncurry step) (recipies, elves)

solve1 :: Int -> String
solve1 = (concatMap show ... (.) <$> S.drop <*> bar) <$> id <*> const steps

-- What are the scores of the ten recipes immediately after the number of recipes in your puzzle input?
solution1 = solve1 input
-- 8610321414

isReversedSuffix :: [Int] -> Recipies -> Bool
isReversedSuffix = (.) <$> (==) <*> lastnReversed . length

quux2 :: [Int] -> Recipies -> Int
quux2 = if' <$$>>> isReversedSuffix
               <*< length ... arg2
               <*< pred . length ... arg2

quux3 :: [Int] -> Recipies -> Bool
quux3 = (&&) <$$>> ((.) <$> (/=) <*> tail ... lastnReversed . succ . length)
               <*< not ... isReversedSuffix

solve2 :: Int -> Int
solve2 = flip (compose3 <$> flip (-) . length <*> quux2 <*> head ... dropWhile . quux3) (fmap fst steps) . reverse . toDigits

-- How many recipes appear on the scoreboard to the left of the score sequence in your puzzle input?
solution2 = solve2 input
-- 20258123
