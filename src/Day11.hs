module Day11 where

import           Control.Arrow    ((&&&))
import           Data.Function    (on)
import           Data.List        (find, maximumBy, sortBy)
import qualified Data.Map.Strict  as M
import           Data.Maybe       (fromJust)
import           Data.Tuple.Extra (both)
import           Prelude          hiding (subtract)


input = 6392

x = fst
y = snd

rackID = (+10) . x

begin = uncurry (*) . (y &&& rackID)
increaseBy serial = (+ serial)
keepHundreds = (`mod` 10) . (`div` 100)
subtract val = val - 5

powerLevel serial = subtract . keepHundreds . uncurry (*) . (increaseBy serial . begin &&& rackID)

grid minx maxx miny maxy = [(x,y) | x <- [minx..maxx], y <- [miny..maxy]]

group size (x,y) = grid x (x+size-1) y (y+size-1)

legalCoordinatesFor size (x,y) = x >= 1 && x <= (300 - size + 1) && y >= 1 && y <= (300 - size + 1)

groupPower ps size = sum . fmap (ps M.!) . group size

toPowerMap serial = M.fromList . fmap (id &&& powerLevel serial)

data Result = Result { coord :: (Int,Int), size :: Int, power :: Int }

withGroupPower ps size = fmap (\coord -> Result coord size $ groupPower ps size coord) . filter (legalCoordinatesFor size)

solve serial groupSizes = maximumBy (compare `on` power) . concat . (\(cs,ps) -> traverse (withGroupPower ps) groupSizes cs) . (id &&& toPowerMap serial)

solve1 serial = coord . solve serial [3]

-- What is the X,Y coordinate of the top-left fuel cell of the 3x3 square with the largest total power?
solution1 = solve1 input $ grid 1 300 1 300
-- 20,58

solve2 serial = (\(Result (x,y) size _) -> (x,y,size)) . solve serial [1..300]

-- What is the X,Y,size identifier of the square with the largest total power?
solution2 = solve2 input $ grid 1 300 1 300
-- 233,268,13