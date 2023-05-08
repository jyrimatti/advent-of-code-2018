module Day22 where

import           Algorithm.Search (dijkstra)
import           Control.Applicative (liftA2)
import           Control.Conditional (if')
import           Data.Bifunctor (first, second)
import           Data.Foldable (length)
import           Data.FoldApp (allOf, listOf)
import           Data.Function (on)
import           Data.Maybe (fromJust)
import           Data.Sequence (Seq, adjust', index, replicate, take, update)
import           Universum ((...))
import           Util ((<$$$>>), (<$$$>>>), (<$$>>), (<$$>>>), (<$$>>>>), (<&>>)
                     , (<*<), arg2, arg31, arg32, arg33, compose3, const2
                     , const3, flip2, (<&), (&>))
import           Prelude hiding (replicate, take, (!!))


depth :: Int
depth = 6084

type RiskLevels = Seq (Seq Int)
type Coordinate = (Int, Int)

target :: Coordinate
target = (14,709)

(!!) :: RiskLevels -> Coordinate -> Int
(!!) = flip . uncurry $ flip index &> (.) <& flip index

geoindex :: RiskLevels -> Coordinate -> Int
geoindex = if' <$$>>> (== (0,0))     ... arg2
                  <*< const2 0 $
           if' <$$>>> (== target)    ... arg2
                  <*< const2 0 $
           if' <$$>>> (== 0)   . snd ... arg2
                  <*< (*16807) . fst ... arg2 $
           if' <$$>>> (== 0)   . fst ... arg2
                  <*< (*48271) . snd ... arg2
                  <*< ( (*) <$$>> id &> (!!) <& first pred
                              <*< id &> (!!) <& second pred)

erosion :: RiskLevels -> Coordinate -> Int
erosion = (`mod` 20183) . (+ depth) ... geoindex

riskLevel :: Int -> Int
riskLevel = (`mod` 3)

allCoords :: [Coordinate]
allCoords = liftA2 (,) [0..(fst target * 5)] [0..depth-1] 

initRiskLevels :: RiskLevels
initRiskLevels = replicate depth $ replicate (fst target * 5) 0

solve :: RiskLevels
solve = fmap riskLevel <$> foldl reducer initRiskLevels allCoords

reducer :: RiskLevels -> Coordinate -> RiskLevels
reducer = adjust' <$$>>> (update <$$>> fst ... arg2 <*< erosion)
                     <*< snd ... arg2
                     <*< const

solve1 :: Int
solve1 = sum $ sum . take (fst target + 1) <$> take (snd target + 1) solve

-- What is the total risk level for the smallest rectangle that includes 0,0 and the target's coordinates?
solution1 :: Int
solution1 = solve1
-- 10603

next :: Coordinate -> [Coordinate]
next = listOf <$> first succ <*> first pred <*> second succ <*> second pred

inside :: RiskLevels -> Coordinate -> Bool
inside = allOf <$$>>>> (>=0) . snd ... arg2
                   <*< length &> (>) <& snd
                   <*< (>=0) . fst ... arg2
                   <*< length . (`index` 0) &> (>) <& fst

data Equipment = ClimingGear | Torch | Neither
  deriving (Eq,Ord,Show)

properGear :: RiskLevels -> Coordinate -> [Equipment]
properGear = if' <$$>>> (== target) ... arg2
                    <*< const2 [Torch] $
             if' <$$>>> (== 0) ... (!!)
                    <*< const2 [ClimingGear, Torch] $
             if' <$$>>> (== 1) ... (!!)
                    <*< const2 [ClimingGear, Neither]
                    <*< const2 [Torch, Neither]

path :: RiskLevels -> Maybe (Int, [(Coordinate, Equipment)])
path = dijkstra <$> neighbours <*> cost <*> const (== (target,Torch)) <*> const ((0,0),Torch)

neighbours :: RiskLevels -> (Coordinate, Equipment) -> [(Coordinate, Equipment)]
neighbours = compose3 <$> concatMap . (zip <$$>> repeat ... arg2 <*< properGear)
                      <*> filter . inside
                      <*> const (next . fst)

foo :: RiskLevels -> (Coordinate, Equipment) -> (Coordinate, Equipment) -> Bool
foo = (&&) <$$$>> const ((/=) `on` snd)
              <*< (elem <$$$>> snd ... arg33
                           <*< (properGear <$$$>> arg31 <*< fst ... arg32) )

cost :: RiskLevels -> (Coordinate, Equipment) -> (Coordinate, Equipment) -> Int
cost = if' <$$$>>> foo
               <*< const3 (1+7) $
       if' <$$$>>> flip2 foo
               <*< const3 (1+7) $
       if' <$$$>>> const ((==) `on` snd)
               <*< const3 1
               <*< const3 9999999999

-- What is the fewest number of minutes you can take to reach the target?
solution2 :: Int
solution2 = fst $ fromJust $ path solve
-- 952