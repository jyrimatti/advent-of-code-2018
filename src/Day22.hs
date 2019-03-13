{-# LANGUAGE TupleSections #-}
module Day22 where

import Data.Maybe (fromJust)
import Prelude hiding (replicate,(!!),length,head,take)
import Data.Sequence hiding (filter,zip)
import           Algorithm.Search (dijkstra)

depth = 6084
target = (14,709)
--depth = 510
--target = (10,10)

s !! (x,y) = (`index` x) . (`index` y) $ s

geoindex _ (0,0) = 0
geoindex _ c | c == target = 0
geoindex _ (x,0) = x*16807
geoindex _ (0,y) = y*48271
geoindex erosions (x,y) = (erosions !! (x-1,y)) * (erosions !! (x,y-1))

erosion erosions = (`mod` 20183) . ( + depth) . geoindex erosions

riskLevel = (`mod` 3)

allCoords = [(x,y) | y <- [0..depth-1], x <- [0..(fst target * 5)]] -- maybe *5 is wide enough...

initRiskLevels = replicate depth $ replicate (fst target * 5) 0

solve = fmap riskLevel <$> foldl (\erosions (x,y) -> adjust' (update x $ erosion erosions (x,y)) y erosions) initRiskLevels allCoords

solve1 = sum $ sum . take (fst target + 1) <$> take (snd target + 1) solve

-- What is the total risk level for the smallest rectangle that includes 0,0 and the target's coordinates?
solution1 = solve1
-- 10603

next (x,y) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

inside map (x,y) = y >= 0 && y < length map && x >= 0 && x < (length . (`index` 0)) map

data Equipment = ClimingGear | Torch | Neither deriving (Eq,Ord,Show)

properGear          _ c | c == target          = [Torch]
properGear riskLevels c | riskLevels !! c == 0 = [ClimingGear, Torch]
properGear riskLevels c | riskLevels !! c == 1 = [ClimingGear, Neither]
properGear riskLevels c | riskLevels !! c == 2 = [Torch, Neither]

path riskLevels = dijkstra neighbours cost (== (target,Torch)) ((0,0),Torch)
  where neighbours = concatMap (zip <$> repeat <*> properGear riskLevels) . filter (inside riskLevels) . next . fst
        cost (c1,e1) (_,e2)  | e1 /= e2 && e2 `elem` properGear riskLevels c1 = 1 + 7
        cost (_,e1) (c2,e2)  | e1 /= e2 && e1 `elem` properGear riskLevels c2 = 1 + 7
        cost (_,e1) (_,e2)   | e1 == e2                                       = 1
        cost _ _                                                              = 9999999999

-- What is the fewest number of minutes you can take to reach the target?
solution2 = fst $ fromJust $ path solve
-- 952