module Day18 where

import           Control.Arrow       ((&&&))
import           Data.List           (iterate')
import qualified Data.Matrix.Unboxed as M
import           Data.Matrix.Unboxed (Matrix, (!))
import qualified Data.Set            as S
import qualified Data.Vector.Unboxed as V


input = lines <$> readFile  "input/input18.txt"

open = '.'
tree = '|'
lumberyard = '#'

type Map = Matrix Char

showMap :: Map -> [String]
showMap = fmap V.toList . M.toColumns

inrange map (x,y) = y >= 0 && y < M.cols map && x >= 0 && x < M.rows map

adjacent (x,y) = filter (/= (x,y)) [(x+a,y+b) | a <- [-1,0,1], b <- [-1,0,1]]

adj map = fmap (map !) . filter (inrange map) . adjacent

magic :: Map -> (Int,Int) -> Char
magic map c | map ! c == open       && length (filter (== tree) $ adj map c) >= 3                   = tree
magic map c | map ! c == tree       && length (filter (== lumberyard) $ adj map c) >= 3             = lumberyard
magic map c | map ! c == lumberyard && (lumberyard `notElem` adj map c || tree `notElem` adj map c) = open
magic map c                                                                                         = map ! c

process = iterate' (\m -> M.imap (const . magic m) m) . M.tr . M.fromLists

solve = fmap (((*) <$> length . filter (== tree) <*> length . filter (== lumberyard)) . M.toList) . process

-- What will the total resource value of the lumber collection area be after 10 minutes?
solution1 = (!! 10) . solve <$> input
-- 653184

-- What will the total resource value of the lumber collection area be after 1000000000 minutes?
solution2 = (!!) <$> solve <*> ((\(a,b) -> a + ((1000000000 - a) `rem` (b-a))) . head . fmap (\(_,a,b) -> (a,b)) . filter (\(_,a,b) -> a > 0 && b > 0) . scanl (\(s,a,b) (i,m) -> let ms = M.toList m in if ms `S.member` s then (S.empty,if a == 0 then i else a,if a > 0 && b == 0 then i else b) else (S.insert ms s, a,b)) (S.empty,0,0) . zip [0..] . process) <$> input
-- 169106

debug i = fmap showMap . take i . process <$> input >>= mapM_ ((>> putStrLn "") . mapM_ putStrLn)