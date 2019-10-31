module Day14 where

import           Data.Foldable    (toList)
import           Data.List        (isPrefixOf, isSuffixOf, iterate', tails)
import qualified Data.Sequence    as S
import           Data.Sequence    ((><), (|>))
import           Data.Tuple.Extra (both)
import           Prelude          hiding ((++))


input = 607331

newtype Elf = Elf {
    current :: Int
} deriving Show

type Recipies = S.Seq Int
(!) = S.index

recipies = S.fromList [3,7]

elves = (Elf 0, Elf 1)

toDigits :: Int -> [Int]
toDigits num = read <$> ((:[]) <$> show num)

newRecipies :: Recipies -> (Elf,Elf) -> [Int]
newRecipies recipies (Elf current1,Elf current2) = toDigits ((recipies ! current1) + (recipies ! current2))

updateRecipie recipies elf@(Elf current) = elf { current = (current + (recipies ! current) + 1) `mod` length recipies }

step :: Recipies -> (Elf,Elf) -> (Recipies,(Elf,Elf))
step recipies elves = let
    updatedRecipies = case newRecipies recipies elves of
        [a,b] -> recipies |> a |> b
        [a] -> recipies |> a
  in
    (updatedRecipies, both (updateRecipie updatedRecipies) elves)

lastnReversed :: Int -> S.Seq Int -> [Int]
lastnReversed 0 seq = []
lastnReversed 1 seq = case S.viewr seq of
    _ S.:> r -> [r]
    S.EmptyR -> []
lastnReversed n seq = case S.viewr seq of
    s S.:> r -> r : lastnReversed (n-1) s
    S.EmptyR -> []

solve1 numRecipies = concatMap show . S.drop numRecipies . fst . fmap fst . head . dropWhile ((< numRecipies + 10) . length . fst) $ iterate' (uncurry step) (recipies, elves)

-- What are the scores of the ten recipes immediately after the number of recipes in your puzzle input?
solution1 = solve1 input
-- 8610321414

solve2 input = let
    digits = reverse $ toDigits input
    dgs = length digits
  in
    (\x -> x - dgs) . (\xs -> if lastnReversed dgs xs == digits then length xs else length xs - 1) . head . dropWhile ((&&) <$> (/= digits) . tail . lastnReversed (dgs+1) <*> (/= digits) . lastnReversed dgs) . fmap fst $ iterate' (uncurry step) (recipies, elves)

-- How many recipes appear on the scoreboard to the left of the score sequence in your puzzle input?
solution2 = solve2 input
-- 20258123
