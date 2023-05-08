module Day11 where

import           Control.Applicative (liftA2)
import           Control.Arrow ((&&&))
import           Data.FoldApp (allOf)
import           Data.List.Extra (maximumOn)
import qualified Data.Map.Strict as M
import           Universum ((...))
import           Util ((<$$$>>>), (<$$>>), (<$$>>>), (<$$>>>>), (<&>>), (<*<), arg2, arg31, arg33, (<$$$>>), const2, argDrop3, argDrop1, (<&), (&>))


type Coordinate = (Int,Int)

input :: Int
input = 6392

rackID :: Coordinate -> Int
rackID = (+10) . fst

beginPowerLevel :: Coordinate -> Int
beginPowerLevel = (*) <$> snd <*> rackID

keepHundreds :: Int -> Int
keepHundreds = (`mod` 10) . (`div` 100)

powerLevel :: Int -> Coordinate -> Int
powerLevel = subtract 5 . keepHundreds ... (*) <$$>> id &> (+) <& beginPowerLevel
                                                 <*< rackID ... arg2

grid :: Coordinate -> Coordinate -> [Coordinate]
--grid (minx, maxx) (miny, maxy) = [(x,y) | x <- [minx..maxx], y <- [miny..maxy]]
--grid = liftA2 (,) <&>> uncurry enumFromTo <*< uncurry enumFromTo
grid = uncurry enumFromTo &> liftA2 (,) <& uncurry enumFromTo

group :: Int -> Coordinate -> [Coordinate]
--group size (x,y) = grid (x, (x+size-1)) (y, (y+size-1))
group = grid <$$>> ((,) <$$>> fst ... arg2 <*< pred &> (+) <& fst)
               <*< ((,) <$$>> snd ... arg2 <*< pred &> (+) <& snd)

legalCoordinatesFor :: Int -> Coordinate -> Bool
--legalCoordinatesFor size (x,y) = 1 <= x && (300 - size + 1) >= x && 1 <= y && (300 - size + 1) >= y
legalCoordinatesFor = allOf <$$>>>> (1 <=) . fst ... arg2
                                <*< (300 + 1 -) &> (>=) <& fst
                                <*< (1 <=) . snd ... arg2
                                <*< (300 + 1 -) &> (>=) <& snd

groupPower :: M.Map Coordinate Int -> Int -> Coordinate -> Int
--groupPower m a b = sum $ fmap (m M.!) $ group a b
--groupPower m = sum . fmap (m M.!) ... group
--groupPower = sum ... ((.) <&>> valuesOfKeys <*< group)
--groupPower   = sum ... valuesOfKeys &> (.) <& group
groupPower = (... group) . (sum ... valuesOfKeys)

valuesOfKeys :: Ord k => M.Map k v -> [k] -> [v]
valuesOfKeys = fmap . (M.!)

toPowerMap :: (Coordinate -> Int) -> [Coordinate] -> M.Map Coordinate Int
--toPowerMap fPowerLevel cs = M.fromList $ fmap (\x -> (x, fPowerLevel x)) cs
--toPowerMap fPowerLevel = M.fromList . fmap (id &&& fPowerLevel)
toPowerMap = M.fromList ... fmap . (id &&&)

data Result = Result {
    size :: Int,
    coord :: Coordinate,
    power :: Int
}

withGroupPower :: M.Map Coordinate Int -> Int -> [Coordinate] -> [Result]
--withGroupPower ps size = fmap (mkResult ps size) . filter (legalCoordinatesFor size)
--withGroupPower ps = fmap <$$>> mkResult ps ... const <*< filter . legalCoordinatesFor
--withGroupPower = (.) <$$>> (fmap .) . flip . (Result <$$>>> arg2 <*< const <*<) . flip . groupPower
--                       <*< const (filter . legalCoordinatesFor)
withGroupPower = fmap <$$$>> argDrop3 mkResult <*< argDrop1 (filter . legalCoordinatesFor)
-- Would be nice to be able to customize the distribution of arguments,
-- since here: "first args 1&2 to the left, args 2&3 to the right"

mkResult :: M.Map Coordinate Int -> Int -> Coordinate -> Result
--mkResult ps = id <$$>> Result <*< groupPower ps
mkResult = (id <$$>> Result <*<) . groupPower

calculate :: [Int] -> Int -> [Coordinate] -> [[Result]]
calculate = traverse <$$$>>> const (withGroupPower ... toPowerMap . powerLevel) <*< arg31 <*< arg33
-- "args 2&3 to the first, arg 1 to the middle, arg3 to the last"

solve :: [Int] -> Int -> [Coordinate] -> Result
solve = maximumOn power . concat ... calculate

solve1 :: Int -> [Coordinate] -> Coordinate
solve1 = coord ... solve [3]

-- What is the X,Y coordinate of the top-left fuel cell of the 3x3 square with the largest total power?
solution1 :: Coordinate
solution1 = solve1 input $ grid (1, 300) (1, 300)
-- 20,58

solve2 :: Int -> [Coordinate] -> (Int, Int, Int)
solve2 = ((,,) <$> fst . coord <*> snd . coord <*> size) ... solve [1..300]

-- What is the X,Y,size identifier of the square with the largest total power?
solution2 :: (Int, Int, Int)
solution2 = solve2 input $ grid (1, 300) (1, 300)
-- 233,268,13