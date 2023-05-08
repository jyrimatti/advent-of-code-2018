{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Day14 where

import           GHC.Generics (Generic)
import           Control.Conditional (if')
import           Control.Lens (over)
import           Data.Generics.Product (field)
import           Data.List (iterate', singleton)
import           Data.Maybe (fromJust, maybeToList)
import qualified Data.Sequence as S
import           Data.Sequence ((><), (|>))
import           Data.Tuple.Extra (both)
import           Universum ((...))
import           Util ((<$$>>), (<$$>>>), (<&>>), (<*<), arg2, compose3, const2, (<&), (&>))


input :: Int
input = 607331

newtype Elf = Elf {
    current :: Int
} deriving (Show, Generic)

type Recipies = S.Seq Int

(!) :: S.Seq a -> Int -> a
(!) = S.index

recipies :: S.Seq Int
recipies = S.fromList [3,7]

elves :: (Elf, Elf)
elves = (Elf 0, Elf 1)

toDigits :: Int -> [Int]
toDigits = fmap (read . singleton) . show

newRecipies :: Recipies -> (Elf,Elf) -> [Int]
--newRecipies recipies (Elf current1,Elf current2) = toDigits ((recipies ! current1) + (recipies ! current2))
newRecipies = toDigits ... (+) <$$>> id &> (!) <& current . fst
                                 <*< id &> (!) <& current . snd

updateRecipie :: Recipies -> Elf -> Elf
--updateRecipie recipies elf@(Elf current) = elf { _current = (current + (recipies ! current) + 1) `mod` length recipies }
updateRecipie = modifier &> over (field @"current") <& id

modifier :: Recipies -> Int -> Int
modifier = mod <$$>> succ ... ((+) <$$>> arg2 <*< (!))
                 <*< length ... const

updatedRecipies :: Recipies -> (Elf, Elf) -> Recipies
--updatedRecipies recipies elves = case newRecipies recipies elves of
--        [a,b] -> recipies |> a |> b
--        [a]   -> recipies |> a
updatedRecipies = (><) <$$>> const <*< S.fromList ... newRecipies

step :: Recipies -> (Elf,Elf) -> (Recipies,(Elf,Elf))
step = ((,) <$$>> const <*< (updateRecipie &> both <& id)) <$$>> updatedRecipies <*< arg2

-- last element of a Seq without pattern matching
seqLast :: S.Seq a -> a
seqLast = fromJust ... S.lookup <$> pred . length <*> id

-- init of a Seq without pattern matching
seqInit :: S.Seq a -> S.Seq a
seqInit = fromJust ... S.lookup <$> pred . length <*> S.inits

foo2 :: Int -> S.Seq Int -> [Int]
foo2 = if' <$$>>> null ... arg2
              <*< const2 []
              <*< ((:) <$$>> seqLast ... arg2 <*< pred &> lastnReversed <& seqInit)

lastnReversed :: Int -> Recipies -> [Int]
lastnReversed = if' <$$>>> (== 0) ... const
                       <*< const2 [] $
                if' <$$>>> (== 1) ... const
                       <*< maybeToList . (S.lookup <$> pred . length <*> id) ... arg2
                       <*< foo2

bar :: (Int -> Bool) -> [(S.Seq a, (b, b1))] -> S.Seq a
--bar pred = fst . head . dropWhile (pred . length . fst)
--bar = fst . head ... dropWhile . (. length . fst)
bar = (. length) &> head ... dropWhile <& fmap fst

steps :: [(Recipies, (Elf, Elf))]
steps = iterate' (uncurry step) (recipies, elves)

solve1 :: Int -> String
solve1 = (concatMap show ... (.) <$> S.drop <*> bar . (>) . (+10)) <$> id <*> const steps

-- What are the scores of the ten recipes immediately after the number of recipes in your puzzle input?
solution1 :: String
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
--solve2 input = let
--    digits = reverse $ toDigits input
--    dgs = length digits
--  in
--    (\x -> x - dgs) . (\xs -> if lastnReversed dgs xs == digits then length xs else length xs - 1) . head . dropWhile ((&&) <$> (/= digits) . tail . lastnReversed (dgs+1) <*> (/= digits) . lastnReversed dgs) $ fmap fst steps
solve2 = flip (compose3 <$> subtract . length <*> quux2 <*> head ... dropWhile . quux3) (fmap fst steps) . reverse . toDigits

-- How many recipes appear on the scoreboard to the left of the score sequence in your puzzle input?
solution2 :: Int
solution2 = solve2 input
-- 20258123
