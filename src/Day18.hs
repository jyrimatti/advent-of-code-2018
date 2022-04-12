{-# LANGUAGE TupleSections #-}
module Day18 where

import           Control.Applicative (liftA2)
import           Control.Arrow       ((&&&))
import           Control.Conditional (if',ToBool)
import           Data.Bifunctor (bimap)
import           Data.FoldApp (allOf)
import           Data.List           (iterate')
import qualified Data.Matrix.Unboxed as M
import           Data.Matrix.Unboxed (Matrix, (!))
import qualified Data.Set            as S
import           Data.Tuple.Extra (both, fst3, snd3, thd3)
import qualified Data.Vector.Unboxed as V
import           Universum.VarArg ((...))
import           Util
import Data.Composition ((.*), (.**))

input :: IO [String]
input = lines <$> readFile  "input/input18.txt"

open :: Char
open = '.'

tree :: Char
tree = '|'

lumberyard :: Char
lumberyard = '#'

type Map = Matrix Char

showMap :: Map -> [String]
showMap = fmap V.toList . M.toColumns

inrange :: Map -> (Int, Int) -> Bool
inrange = allOf <$$>>>> (>= 0) . snd ... arg2
                    <*< ((>) <&>> M.cols <*< snd)
                    <*< (>= 0) . fst ... arg2
                    <*< ((>) <&>> M.rows <*< fst)

adjacent :: (Int, Int) -> [(Int, Int)]
adjacent = filter <$> (/=) <*> uncurry (flip fmap movements .* bimap) . both (flip ($))

movements :: [(Int -> Int, Int -> Int)]
movements = both (+) <$> liftA2 (,) [-1,0,1] [-1,0,1]

adj :: Map -> (Int, Int) -> [Char]
adj = (. adjacent) . ((.) <$> fmap . (!) <*> filter . inrange)

magic :: Map -> (Int,Int) -> Char
magic = if' <$$>>> ( (&&) <$$>> (== open) ... (!) <*< enough tree ... adj )
               <*< const2 tree $
        if' <$$>>> ( (&&) <$$>> (== tree) ... (!) <*< enough lumberyard ... adj )
               <*< const2 lumberyard $
        if' <$$>>> ( (&&) <$$>> (== lumberyard) ... (!) <*< ( (||) <$$>> (lumberyard `notElem`) ... adj
                                                                     <*< (tree       `notElem`) ... adj) )
               <*< const2 open
               <*< (!)

enough :: Char -> [Char] -> Bool
enough = (>= 3) . length ... filter . (==)

process :: [[Char]] -> [Map]
process = iterate' (M.imap <$> const ... magic <*> id) . M.tr . M.fromLists

solve :: [[Char]] -> [Int]
solve = fmap (((*) <$> length . filter (== tree) <*> length . filter (== lumberyard)) . M.toList) . process

-- What will the total resource value of the lumber collection area be after 10 minutes?
solution1 = (!! 10) . solve <$> input
-- 653184

bar :: Int -> Int -> Int -> String -> S.Set String -> (S.Set String, Int, Int)
bar = if' <$$$$$>>> (S.member <$$$$$>> arg54 <*< arg55)
                <*< (const ... (argDrop .* (S.empty,,)) <$$$>> (if' <$$$>>> (== 0) .** arg31 <*< arg33 <*< arg31)
                                                            <*< (if' <$$$>>> ((&&) <$$$>> ((>0) ... arg31) <*< (==0) ... arg32) <*< arg33 <*< arg32 ))
                <*< ((,,) <$$$$$>>> (S.insert <$$$$$>> arg54 <*< arg55) <*< arg51 <*< arg52)

foo :: (S.Set String, Int, Int) -> (Int,Map) -> (S.Set String, Int, Int)
foo = bar <$$>>>>> snd3 ... arg1
               <*< thd3 ... arg1
               <*< fst ... arg2
               <*< M.toList . snd ... arg2
               <*< fst3 ... arg1

quux :: Int -> Int -> Int
quux = (+) <$$>> arg1 <*< (rem <$$>> (1000000000 -) ... arg1 <*< flip (-))

baz :: [String] -> Int
baz = uncurry quux . head . fmap ((,) <$> snd3 <*> thd3) . filter ((&&) <$> (> 0) . snd3 <*> (> 0) . thd3) . scanl foo (S.empty,0,0) . zip [0..] . process

-- What will the total resource value of the lumber collection area be after 1000000000 minutes?
solution2 = (!!) <$> solve <*> baz <$> input

-- 169106

debug i = fmap showMap . take i . process <$> input >>= mapM_ ((>> putStrLn "") . mapM_ putStrLn)