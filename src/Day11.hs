module Day11 where

import           Control.Applicative (liftA2)
import           Control.Arrow    ((&&&))
import           Data.FoldApp (allOf)
import           Data.Function    (on)
import           Data.List        (find, maximumBy, sortBy)
import           Data.List.Extra (maximumOn)
import qualified Data.Map.Strict  as M
import           Data.Maybe       (fromJust)
import           Data.Tuple.Extra (both)
import           Prelude          hiding (subtract)
import           Universum.VarArg ((...))
import Util
    ( (<$$$>>>),
      (<$$>>),
      (<$$>>>),
      (<$$>>>>),
      (<&>>),
      (<*<),
      arg2,
      arg31,
      arg33 )

input :: Int
input = 6392

x :: (a, b) -> a
x = fst
y :: (a, b) -> b
y = snd

rackID :: (Int, b) -> Int
rackID = (+10) . x

begin :: (Int, Int) -> Int
begin = (*) <$> y <*> rackID

keepHundreds :: Int -> Int
keepHundreds = (`mod` 10) . (`div` 100)

subtract :: Int -> Int
subtract = flip (-) 5

powerLevel :: Int -> (Int, Int) -> Int
powerLevel = subtract . keepHundreds ... (*) <$$>> ((+) <&>> id <*< begin) <*< rackID ... arg2

grid :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
--grid (minx, maxx) (miny, maxy) = [(x,y) | x <- [minx..maxx], y <- [miny..maxy]]
grid = liftA2 (,) <&>> uncurry enumFromTo <*< uncurry enumFromTo

group :: Int -> (Int, Int) -> [(Int, Int)]
--group size (x,y) = grid (x, (x+size-1)) (y, (y+size-1))
group = grid <$$>> ((,) <$$>> fst ... arg2 <*< ((+) <&>> pred <*< fst))
               <*< ((,) <$$>> snd ... arg2 <*< ((+) <&>> pred <*< snd))

legalCoordinatesFor :: Int -> (Int, Int) -> Bool
--legalCoordinatesFor size (x,y) = x >= 1 && x <= (300 - size + 1) && y >= 1 && y <= (300 - size + 1)
legalCoordinatesFor = allOf <$$>>>> (>= 1) . fst ... arg2
                                <*< ( (<=) <&>> id <*< (300 + 1 -) . fst )
                                <*< (>= 1) . snd ... arg2
                                <*< ( (<=) <&>> id <*< (300 + 1 -) . snd )

groupPower :: M.Map (Int, Int) Int -> Int -> (Int, Int) -> Int
groupPower = sum ... ((.) <&>> fmap . (M.!) <*< group)

toPowerMap :: Int -> [(Int, Int)] -> M.Map (Int, Int) Int
toPowerMap = (M.fromList .) . fmap . (id &&&) . powerLevel

data Result = Result {
    coord :: (Int,Int),
    size :: Int,
    power :: Int
}

withGroupPower :: M.Map (Int, Int) Int -> Int -> [(Int, Int)] -> [Result]
--withGroupPower ps size = fmap (\coord -> Result coord size $ groupPower ps size coord) . filter (legalCoordinatesFor size)
withGroupPower = (.) <$$>> (fmap .) . flip . (Result <$$>>> const <*< arg2 <*<) . flip . groupPower
                       <*< const (filter . legalCoordinatesFor)

solve :: [Int] -> Int -> [(Int, Int)] -> Result
solve = maximumOn power . concat ... (traverse <$$$>>> const (withGroupPower ... toPowerMap) <*< arg31 <*< arg33)

solve1 :: Int -> [(Int, Int)] -> (Int, Int)
solve1 serial = coord . solve [3] serial

-- What is the X,Y coordinate of the top-left fuel cell of the 3x3 square with the largest total power?
solution1 :: (Int, Int)
solution1 = solve1 input $ grid (1, 300) (1, 300)
-- 20,58

solve2 :: Int -> [(Int, Int)] -> (Int, Int, Int)
solve2 = ((,,) <$> fst . coord <*> snd . coord <*> size) ... solve [1..300]

-- What is the X,Y,size identifier of the square with the largest total power?
solution2 :: (Int, Int, Int)
solution2 = solve2 input $ grid (1, 300) (1, 300)
-- 233,268,13