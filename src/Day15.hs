{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
module Day15 where

import           Algorithm.Search (bfs)
import           Control.Arrow ((&&&))
import           Data.Bifunctor (second,first,bimap)
import           Data.Foldable (toList)
import           Data.Function (on)
import           Data.List (sort,nub,cycle,iterate',(\\),sortBy,nubBy,groupBy,sortOn,zip)
import           Data.List.Extra (groupSortBy)
import qualified Data.Matrix as M
import           Data.Matrix (Matrix, (!))
import           Data.Maybe (isJust,fromJust,isNothing,listToMaybe,maybeToList,catMaybes,fromMaybe,mapMaybe)
import           Data.Ord (comparing)
import qualified Data.Sequence as S
import           Data.Sequence (Seq)
import           Data.Tuple (swap)
import qualified Data.Vector as V
import           Data.Vector (Vector)
import           Prelude hiding (Either(Left,Right),round)
import           Text.Parsec (parse,many,many1,optional,(<|>))
import           Text.Parsec.Char (char,space,string,letter,digit,anyChar)
import           Text.Parsec.Combinator (between,sepBy)
import           Text.ParserCombinators.Parsec.Number (int)

inputLines = fmap lines . readFile 

input = inputLines "input/input15.txt"
test1 = inputLines "input/input15_test1.txt"
test2 = inputLines "input/input15_test2.txt"
test3 = inputLines "input/input15_test3.txt"
test4 = inputLines "input/input15_test4.txt"
test5 = inputLines "input/input15_test5.txt"
test6 = inputLines "input/input15_test6.txt"
test7 = inputLines "input/input15_test7.txt"

type Map = Matrix Char

type UnitType = Char

data Unit = Unit {
    unitType :: UnitType,
    location :: (Int,Int),
    power :: Int,
    hp :: Int
} deriving (Show,Eq)

type Units = S.Seq Unit

isGoblin = (== 'G') . unitType
isElf    = (== 'E') . unitType

isAlive = (> 0) . hp

toUnit i 'G' = Just $ Unit 'G' i 3 200
toUnit i 'E' = Just $ Unit 'E' i 3 200
toUnit _  _  = Nothing

findUnits :: Map -> Units
findUnits = S.fromList . catMaybes . concat . M.toLists . M.imap toUnit

find pred = fmap . S.index <$> id <*> S.findIndexL pred

withUnits :: Units -> Map -> Map
withUnits units = M.imap (\coords square -> maybe square unitType $ find ((== coords) . location) units)

withoutUnits :: Map -> Map
withoutUnits = M.map $ \case
    'G' -> '.'
    'E' -> '.'
    x   -> x

targetsFor :: Unit -> Units -> [Unit]
targetsFor (Unit sourceType _ _ _) = filter ((&&) <$> isAlive <*> (/= sourceType) . unitType) . toList

nextLocs (row,col) = [(row-1,col),(row,col-1),(row,col+1),(row+1,col)]

isOpen :: Map -> Units -> (Int,Int) -> Bool
isOpen map _ (x,y) | x < 0 || y < 0 || x > M.rows map || y > M.cols map = False
isOpen map units loc                                                    = map ! loc == '.' && (loc `notElem` fmap location (S.filter isAlive units))

pathToTarget :: Map -> Units -> Unit -> Unit -> Maybe [(Int,Int)]
pathToTarget map units source target = let
    nextStates = filter ((||) <$> isOpen map units <*> (== location target)) . nextLocs
    isSolution = (`elem` (nextLocs $ location target))
    initialState = location source
  in
    bfs nextStates isSolution initialState

pathToNearest :: Map -> Units -> Unit -> [Unit] -> Maybe [(Int,Int)]
pathToNearest map units source = listToMaybe . sortOn (length &&& head) . mapMaybe (pathToTarget map units source)

update :: (Unit -> Unit) -> Unit -> Units -> (Unit,Units)
update f unit = (f unit,) . (S.adjust' f <$> fromJust . S.elemIndexL unit <*> id)

move :: Map -> Unit -> Units -> (Unit,Units)
move map u units = let 
    newLocation unit = maybe (location unit) head (pathToNearest map units unit $ targetsFor unit units)
  in
    update (\unit -> unit { location = newLocation unit }) u units

attack :: Unit -> Units -> Units
attack unit = let
    inRange = (`elem` nextLocs (location unit)) . location
    findTarget = head . sortOn (hp &&& location) . filter inRange . targetsFor unit
  in
    snd . (update (\u -> u { hp = hp u - power unit }) <$> findTarget <*> id)

isAdjacentToTarget unit = (location unit `elem`) . concatMap (nextLocs . location) . targetsFor unit

act :: Map -> Unit -> Units -> Units
act _   unit units | not (isAlive unit)            = units
act _   unit units | isAdjacentToTarget unit units = attack unit units
act map unit units                                 =
    uncurry (\b -> if b then uncurry attack else snd) $ (uncurry isAdjacentToTarget &&& id) $ move map unit units

actUnit :: Map -> Units -> Int -> Units
actUnit map units unitIndex = act map (S.index units unitIndex) units

round :: Map -> Units -> [Units]
round map = fmap (S.filter isAlive) . (take <$> length <*> tail . fmap fst . iterate' (uncurry (actUnit map) &&& (+ 1) . snd) . (,0))

allRounds :: Map -> Units -> [[Units]]
allRounds map = iterate' (round map . S.sortOn location . last) . (: [])

mapAndUnits elfPower = (withoutUnits &&& fmap (withElfPower elfPower) . findUnits) . M.fromLists

unitTypesAtEndOfRound = length . nub . toList . S.sort . fmap unitType . last

notCompleted = (>1) . unitTypesAtEndOfRound . snd

dec x = x - 1

solve :: Int -> [String] -> (Map,(Int,[Units]))
solve elfPower = (fst &&& head . dropWhile notCompleted . zip [0..] . uncurry allRounds) . mapAndUnits elfPower

outcome = (*) <$> sum . fmap hp . last . snd <*> uncurry (+) . bimap dec (dec . unitTypesAtEndOfRound . init)

-- the number of full rounds that were completed (not counting the round in which combat ends) multiplied by the sum of the hit points of all remaining units at the moment combat ends.
solution1 = outcome . snd . solve 3 <$> input
-- 190777

withElfPower pow unit | isGoblin unit = unit
withElfPower pow unit                 = unit { power = pow }

elfsWinWithoutLosses :: Int -> (Int,[Units]) -> Bool
elfsWinWithoutLosses startingElves = (&&) <$> all isElf . last . snd <*> (== startingElves) . length . S.filter isElf . last . snd

-- What is the outcome
solution2 = outcome . fst . head . filter snd . traverse (\i -> curry (snd &&& uncurry elfsWinWithoutLosses) <$> length . S.filter isElf . findUnits . M.fromLists <*> snd . solve i) [4..] <$> input
-- 47388

tests = traverse (fmap $ outcome . snd . solve 3) [test1, test2, test3, test4, test5, test6, test7]

debug d = second M.toLists . last . drop d . (\(map,res) -> (\us -> (hp <$> S.sortOn location us,us `withUnits` map)) <$> concat res) . solve 3 <$> input >>= \(us,map) -> do mapM_ putStrLn map; print us