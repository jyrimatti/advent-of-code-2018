{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
module Day15 where

import           GHC.Generics (Generic)
import           Algorithm.Search (bfs)
import           Control.Arrow ((&&&))
import           Control.Conditional (if')
import           Control.Lens (over, set)
import           Data.Composition ((.**), (.*))
import           Data.Bifunctor (first, second)
import           Data.Foldable (toList)
import           Data.FoldApp (listOf)
import           Data.Function (on)
import           Data.Generics.Product (field)
import           Data.List (iterate', sortOn, zip, singleton)
import           Data.List.Extra (nubOrd)
import qualified Data.Matrix as M
import           Data.Matrix (Matrix, (!))
import           Data.Maybe (catMaybes, fromJust, listToMaybe, mapMaybe)
import qualified Data.Sequence as S
import           Data.Sequence (Seq)
import qualified Data.Vector as V
import           Data.Vector (Vector)
import           Universum ((...))
import           Util ((<$$$$>>), (<$$$$>>>), (<$$$>>), (<$$$>>>), (<$$$>>>>)
                     , (<$$>>), (<$$>>>), (<$$>>>>), (<&>>), (<*<), anyOf, arg2
                     , arg31, arg32, arg33, arg41, arg42, arg43, arg44, const2
                     , const3, (<&), (&>))


inputLines :: FilePath -> IO [String]
inputLines = fmap lines . readFile

input :: IO [String]
input = inputLines "input/input15.txt"

test1, test2, test3, test4, test5, test6, test7 :: IO [String]
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
    power    :: Int,
    hp       :: Int,
    location :: (Int,Int)
} deriving (Show,Eq,Generic)

type Units = S.Seq Unit

isGoblin :: Unit -> Bool
isGoblin = (== 'G') . unitType

isElf :: Unit -> Bool
isElf    = (== 'E') . unitType

isAlive :: Unit -> Bool
isAlive = (> 0) . hp

toUnit :: (Int, Int) -> Char -> Maybe Unit
toUnit = if' <$$>>> (== 'G') ... arg2
                <*< Just . Unit 'G' 3 200 ... const $
         if' <$$>>> (== 'E') ... arg2
                <*< Just . Unit 'E' 3 200 ... const
                <*< const2 Nothing

findUnits :: Map -> Units
findUnits = S.fromList . catMaybes . concat . M.toLists . M.imap toUnit

find :: (Unit -> Bool) -> Units -> Maybe Unit
find = fmap . S.index <$$>> arg2 <*< S.findIndexL

findUnitWithCoords :: Units -> (Int, Int) -> Maybe Unit
findUnitWithCoords = id &> flip find <& (. location) . (==)

withUnits :: Units -> Map -> Map
withUnits = M.imap . (maybe <$$$>>> arg33
                                <*< const3 unitType
                                <*< const ... findUnitWithCoords)

withoutUnits :: Map -> Map
withoutUnits = M.map $ if' <$> (== 'G')
                           <*> const '.' <*> (
                       if' <$> (== 'E')
                           <*> const '.' <*>
                               id)

unitTypeDiffersFrom :: Unit -> Unit -> Bool
unitTypeDiffersFrom = (/=) `on` unitType

targetsFor :: Units -> Unit -> [Unit]
targetsFor = id &> toList ... flip S.filter <& ((&&) <$> isAlive <*> ) . unitTypeDiffersFrom

nextLocs :: (Int, Int) -> [(Int, Int)]
nextLocs = uncurry $ listOf <$$>>>> first  pred .* (,)
                                <*< second pred .* (,)
                                <*< second succ .* (,)
                                <*< first  succ .* (,)

isOpen :: Map -> Units -> (Int,Int) -> Bool
--isOpen map _ (x,y) | x < 0 || y < 0 || x > M.rows map || y > M.cols map = False
--isOpen map units loc                                                    = map ! loc == '.' && (loc `notElem` fmap location (S.filter isAlive units))
isOpen = if' @Bool <$$$>>> (anyOf <$$$>>>> (<0) . fst ... arg33
                                       <*< (<0) . snd ... arg33
                                       <*< ((>) <$$$>> fst ... arg33 <*< M.rows ... arg31)
                                       <*< ((>) <$$$>> snd ... arg33 <*< M.cols ... arg31))
                       <*< const3 False
                       <*< ((&&) <$$$>> ((== '.') ... ((!) <$$$>> arg31 <*< arg33))
                                    <*< (notElem <$$$>> arg33 <*< fmap location . S.filter isAlive ... arg32))

isSolution :: Unit -> (Int, Int) -> Bool
isSolution = flip elem . nextLocs . location

nextStates :: Map -> Units -> Unit -> (Int, Int) -> [(Int, Int)]
nextStates = filter <$$$$>> ((<*>) . fmap (||) <$$$$>> (isOpen <$$$$>> arg41 <*< arg42) <*< (==) . location ... arg43)
                        <*< nextLocs ... arg44

pathToTarget :: Map -> Units -> Unit -> Unit -> Maybe [(Int,Int)]
pathToTarget = bfs <$$$$>>> (nextStates <$$$$>>> arg41 <*< arg42 <*< arg44)
                        <*< isSolution ... arg44
                        <*< location ... arg43

pathToNearest :: Map -> Units -> Unit -> [Unit] -> Maybe [(Int,Int)]
pathToNearest = listToMaybe . sortOn (length &&& head) ... (mapMaybe .** pathToTarget)
-- sortOn (firstByThis &&& andThenByThis) xs

adjustElem :: (Unit -> Unit) -> Unit -> Units -> Units
adjustElem = S.adjust' <$$$>>> arg31
                           <*< fromJust ... const S.elemIndexL
                           <*< arg33

update :: (Unit -> Unit) -> Unit -> Units -> (Units,Unit)
update = (.) <$$>> flip (,) ... ($) -- return with adjusted element (after adjusting)
               <*< adjustElem

newLocation :: Map -> Units -> Unit -> (Int, Int)
newLocation = maybe <$$$>>> location ... arg33
                        <*< const3 head
                        <*< (($) <$$$>> pathToNearest <*< const targetsFor)

move :: Map -> Units -> Unit -> (Units,Unit)
move = update <$$$>>> (($) <$$$>>> arg31 <*< arg33 <*< arg33) .* (set (field @"location") ... newLocation)
                  <*< arg33
                  <*< arg32

inRange :: Unit -> Unit -> Bool
inRange = location &> elem <& nextLocs . location

findTarget :: Units -> Unit -> Unit
findTarget = head . sortOn (hp &&& location) ... (filter <$$>> inRange ... arg2 <*< targetsFor)

attack :: Units -> Unit -> Units
attack = adjustElem <$$>>> over (field @"hp") . subtract . power ... arg2
                       <*< findTarget
                       <*< const

isAdjacentToTarget :: Units -> Unit -> Bool
isAdjacentToTarget = ($) <$$>> (elem . location ... arg2)
                           <*< concatMap (nextLocs . location) ... targetsFor

act :: Map -> Units -> Unit -> Units
act = if' <$$$>>> not . isAlive ... arg33
              <*< arg32 $
      if' <$$$>>> const isAdjacentToTarget
              <*< const attack
              <*< uncurry afterMove ... move

afterMove :: Units -> Unit -> Units
afterMove = if' <$$>>> isAdjacentToTarget
                   <*< attack
                   <*< const

actUnit :: Map -> Units -> Int -> Units
actUnit = act <$$$>>> arg31
                  <*< arg32
                  <*< const S.index

doIterate :: Map -> Units -> [(Units, Int)]
doIterate = ((,) <$$>> uncurry . actUnit <*< const (succ . snd)) &> iterate' <& (,0)

doRound :: Map -> Units -> [Units]
doRound = fmap (S.filter isAlive) ... take <$$>> length ... arg2
                                           <*< tail . fmap fst ... doIterate

allRounds :: Map -> Units -> [[Units]]
allRounds = (. S.sortOn location . last) . doRound &> iterate' <& singleton

mapAndUnits :: Int -> [String] -> (Map, Units)
mapAndUnits = (. M.fromLists) . ((,) <$$>> const withoutUnits <*< (. findUnits) . fmap . withElfPower)

unitTypesAtEndOfRound :: [Units] -> Int
unitTypesAtEndOfRound = length . nubOrd . fmap unitType . toList . last

notCompleted :: (a, [Units]) -> Bool
notCompleted = (>1) . unitTypesAtEndOfRound . snd

solve :: Int -> [String] -> (Map,(Int,[Units]))
solve = (fst &&& head . dropWhile notCompleted . zip [0..] . uncurry allRounds) ... mapAndUnits

outcome :: (Int, [Seq Unit]) -> Int
outcome = (*) <$> sum . fmap hp . last . snd
              <*> (((+) `on` pred) <$> fst
                                   <*> unitTypesAtEndOfRound . init . snd)

-- the number of full rounds that were completed (not counting the round in which combat ends) multiplied by the sum of the hit points of all remaining units at the moment combat ends.
solution1 :: IO Int
solution1 = outcome . snd . solve 3 <$> input
-- 190777

withElfPower :: Int -> Unit -> Unit
withElfPower = if' <$$>>> isGoblin ... arg2
                      <*< arg2
                      <*< set (field @"power")

onlyElvesRemain :: (a, [Units]) -> Bool
onlyElvesRemain = all isElf . last . snd

noElvesLost :: Int -> (a, [Units]) -> Bool
noElvesLost = id &> (==) <& length . S.filter isElf . last . snd

elfsWinWithoutLosses :: Int -> (Int,[Units]) -> Bool
elfsWinWithoutLosses = (&&) <$$>> const onlyElvesRemain <*< noElvesLost

withElfWinStatus :: Int -> [String] -> ((Int, [Units]), Bool)
withElfWinStatus = ((,) <$$>> arg2 <*< elfsWinWithoutLosses) <$$>> length . S.filter isElf . findUnits . M.fromLists ... arg2
                                                               <*< snd ... solve

-- What is the outcome
solution2 :: IO Int
solution2 = outcome . fst . head . filter snd . traverse withElfWinStatus [4..] <$> input
-- 47388

tests :: IO [Int]
tests = traverse (fmap $ outcome . snd . solve 3) [test1, test2, test3, test4, test5, test6, test7]
