module Day25 where

import           Control.Applicative.Combinators (sepBy)
import           Control.Conditional (if')
import           Data.Foldable (foldl')
import           Data.FoldApp (sumOf)
import           Data.Function (on)
import           Data.List (iterate', zip, singleton)
import           Data.Maybe (fromJust)
import qualified Data.Sequence as S
import           Data.Sequence (Seq(..))
import           Text.Megaparsec (Parsec, parse)
import           Text.Megaparsec.Char (char, space)
import           Text.Megaparsec.Char.Lexer (decimal, signed)
import           Util ((<$$>>>>), onnn, fst4, snd4, thd4, fth4, (<*<), (<$$>>), arg32, (<$$$>>>), arg33, arg31, (<&), (&>), (<$$>>>), arg2, arg1, const2)
import           Universum ((...))


inputLines :: FilePath -> IO [String]
inputLines = fmap lines . readFile 

input :: IO [String]
input = inputLines "input/input25.txt"

test1, test2, test3, test4 :: IO [String]
test1 = inputLines "input/input25_test1.txt"
test2 = inputLines "input/input25_test2.txt"
test3 = inputLines "input/input25_test3.txt"
test4 = inputLines "input/input25_test4.txt"

type Parser = Parsec () String

type Point = (Int,Int,Int,Int)

pointP :: Parser Point
pointP = ((,,,) <$> (!! 0) <*> (!! 1) <*> (!! 2) <*> (!! 3)) <$> (space *> signed space decimal `sepBy` char ',')

point :: String -> Point
point = either undefined id . parse pointP ""

manhattan :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Int
--manhattan (a1,b1,c1,d1) (a2,b2,c2,d2) = abs (a1-a2) + abs (b1-b2) + abs (c1-c2) + abs (d1-d2)
manhattan = (sumOf `onnn` abs) <$$>>>> ((-) `on` fst4)
                                   <*< ((-) `on` snd4)
                                   <*< ((-) `on` thd4)
                                   <*< ((-) `on` fth4)

join :: Seq [Point] -> Point -> Seq [Point]
--join constellations point = case S.findIndexL (any $ (<= 3) . manhattan point) constellations of
--    Just i -> S.adjust' (point :) i constellations
--    Nothing -> constellations :|> [point]
join = maybe <$$>>> id &> (:|>) <& singleton
                <*< (S.adjust' <$$$>>> (:) ... arg32 <*< arg33 <*< arg31)
                <*< (S.findIndexL <$$>> any . ((<= 3) .) . manhattan ... arg2 <*< arg1)

collapse :: [Point] -> [Point] -> Bool
--collapse as bs | as == bs = False 
--collapse as bs = any (<= 3) $ concatMap (\a -> fmap (($ a) . manhattan) bs) as
collapse = if' <$$>>> (==)
                  <*< const2 False $
                      any (<= 3) ... concatMap <$$>> flip (fmap . flip manhattan) ... arg2 <*< arg1

foo :: Seq [Point] -> Seq [Point]
--foo cs = let
--    mi = S.findIndexL (\c -> or $ fmap (($ c) . collapse) cs) cs
--  in
--    case mi of
--        Just i -> let
--            constellationToRemove = S.index cs i
--            constellationToMergeTo = fromJust $ S.findIndexL (collapse constellationToRemove) cs
--          in
--            S.deleteAt i $ S.adjust' (constellationToRemove <>) constellationToMergeTo cs
--        Nothing -> cs
foo = maybe <$> id
            <*> (S.deleteAt <$$>> arg2
                              <*< (S.adjust' <$$>>> (<>) ... S.index
                                                <*< (fromJust ... S.findIndexL <$$>> collapse ... S.index <*< arg1)
                                                <*< arg1))
            <*> (S.findIndexL <$> (or ... fmap <$$>> collapse ... arg2 <*< arg1)
                              <*> id)

skipWhileAdvancing :: [Seq [Point]] -> [Seq [Point]]
skipWhileAdvancing = fmap snd . dropWhile (uncurry (/=)) . (zip <$> id <*> tail)

solve1 :: [String] -> Seq [Point]
solve1 = head . skipWhileAdvancing . iterate' foo . foldl' join S.empty . fmap point

-- How many constellations are formed by the fixed points in spacetime?
solution1 :: IO Int
solution1 = length . solve1 <$> input
-- 375