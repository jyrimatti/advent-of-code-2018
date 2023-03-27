{-# LANGUAGE TupleSections #-}
module Day23 where

import           Control.Applicative (liftA2,liftA3)
import           Control.Arrow              (first, second, (&&&))
import           Control.Monad.Combinators  (between, sepBy)
import           Data.Foldable              (minimumBy)
import           Data.FoldApp (allOf, sumOf, listOf, foldOf)
import           Data.Function              (on)
import           Data.List                  (groupBy, maximumBy, nub,
                                             permutations, sort, sortOn)
import           Data.List.Extra (groupOn)
import           Data.Maybe                 (catMaybes, fromJust)
import           Data.Monoid
import           Data.Ord                   (Down (..), Ord (..), comparing)
import qualified Data.Set                   as S
import           Data.Tuple.Extra           hiding (first, second, (&&&))
import           Text.Megaparsec            (Parsec, optional, parse, parseMaybe, try,
                                             (<|>))
import           Text.Megaparsec.Char       (char, space, string)
import           Text.Megaparsec.Char.Lexer (decimal, signed)
import           Universum.VarArg ((...))
import           Util
import Data.Composition ((.*))

input :: IO [String]
input = lines <$> readFile "input/input23.txt"

test2 :: IO [String]
test2 = lines <$> readFile "input/input23_test2.txt"

test3 :: IO [String]
test3 = lines <$> readFile "input/input23_test3.txt"

type Coord = (Int,Int,Int)

data Nanobot = Nanobot {
    pos :: Coord,
    radius :: Int
} deriving (Show,Eq)

type Parser = Parsec () String

nanobotP :: Parser Nanobot
nanobotP = Nanobot <$> (((,,) <$> head <*> head . tail <*> head . tail . tail) <$> (string "pos=<" *> signed space decimal `sepBy` char ',')) <*> (string ">, r=" *> decimal)

nanobot :: String -> Nanobot
nanobot = fromJust ... parseMaybe nanobotP

manhattan :: Coord -> Coord -> Int
manhattan = (sumOf `onn` abs) <$$>>> ((-) `on` fst3) <*< ((-) `on` snd3) <*< ((-) `on` thd3)

solve1 :: [String] -> Int
solve1 = length . (filter <$> (>=) . radius . fst <*> uncurry fmap . first (manhattan . pos)) . (maximumBy (comparing radius) &&& fmap pos) . fmap nanobot

-- How many nanobots are in range
solution1 = solve1 <$> input
-- 341

roundBot :: Int -> Nanobot -> Nanobot
roundBot = Nanobot <$$>> ((,,) <$$>>> (flip div <&>> id <*< fst3 . pos)
                                  <*< (flip div <&>> id <*< snd3 . pos)
                                  <*< (flip div <&>> id <*< thd3 . pos))
                     <*< ceiling ... (flip (/) <&>> fromIntegral <*< fromIntegral . radius)

limitedRange :: Int -> Int -> Int -> [Int]
limitedRange = enumFromTo <$$$>> (max <$$$>> (*10) ... arg32 <*< ((-) <$$$>> arg31 <*< arg33))
                             <*< (min <$$$>> (+9) . (*10) ... arg32 <*< ((+) <$$$>> arg31 <*< arg33))

coords :: Nanobot -> [Coord]
coords = (filter .* flip (.)) <$> manhattan . pos
                                 <*> flip (<=) . radius
                                 <*> (liftA3 (,,) <$> (enumFromTo <$> ((-) <$> fst3 . pos <*> radius) <*> ((+) <$> fst3 . pos <*> radius))
                                                  <*> (enumFromTo <$> ((-) <$> snd3 . pos <*> radius) <*> ((+) <$> snd3 . pos <*> radius))
                                                  <*> (enumFromTo <$> ((-) <$> thd3 . pos <*> radius) <*> ((+) <$> thd3 . pos <*> radius)))

coords2 :: Coord -> Nanobot -> [Coord]
coords2 = (filter .* flip (.)) <$$>>> manhattan . pos ... arg2
                                     <*< flip (<=) . radius ... arg2
                                     <*< (liftA3 (,,) <$$>>> (limitedRange <$$>>> fst3 . pos ... arg2 <*< fst3 ... const <*< radius ... arg2)
                                                         <*< (limitedRange <$$>>> snd3 . pos ... arg2 <*< snd3 ... const <*< radius ... arg2)
                                                         <*< (limitedRange <$$>>> thd3 . pos ... arg2 <*< thd3 ... const <*< radius ... arg2))

minimumsBy :: (Eq a, Ord b) => (a -> b) -> [a] -> [a]
minimumsBy = nub . head ... (.) <$> groupOn <*> sortOn

maximumsBy :: (Eq a, Ord b) => (a -> b) -> [a] -> [a]
maximumsBy = nub . head ... (.) <$> groupOn <*> sortOn . (Down .)

botsInRange :: (a, b) -> b
botsInRange = snd

distanceToOrigin :: (Coord, b) -> Int
distanceToOrigin = manhattan (0,0,0) . fst

baz :: ([Nanobot] -> [Coord]) -> [Nanobot] -> [(Coord, Int)]
baz = (fmap . (id &&&) . flip inRangeOfBots <$> id <*>)

process :: ([Nanobot] -> [Coord]) -> Int -> [String] -> [(Coord, Int)]
process = (.) <&>> maximumsBy botsInRange ... baz <*< fmap . (. nanobot) . roundBot

start :: Int -> [String] -> [(Coord, Int)]
start = process (nub . concatMap coords)

nearestToOrigin :: [(Coord, Int)] -> [(Coord, Int)]
nearestToOrigin = nub . concat . take 2 . groupOn distanceToOrigin . sortOn distanceToOrigin

cont :: Coord -> Int -> [String] -> [(Coord, Int)]
cont = nearestToOrigin ... process . (nub ... concatMap . coords2)

inRange :: Coord -> Nanobot -> Bool
inRange = (<=) <$$>> (. pos) . manhattan <*< const radius

inRangeOfBots :: Coord -> [Nanobot] -> Int
inRangeOfBots = length ... filter . inRange

toList :: (a, a) -> [a]
toList = uncurry (flip (:) . singleton)

corners :: (Coord,Coord) -> [Coord]
corners = liftA3 (,,) <$> toList . both fst3 <*> toList . both snd3 <*> toList . both thd3

inside :: (Coord,Coord) -> Nanobot -> Bool
inside = allOf <$$>>>>>> ((<=) <&>> fst3 . fst <*< fst3 . pos)
                     <*< ((>=) <&>> fst3 . snd <*< fst3 . pos)
                     <*< ((<=) <&>> snd3 . fst <*< snd3 . pos)
                     <*< ((>=) <&>> snd3 . snd <*< snd3 . pos)
                     <*< ((<=) <&>> thd3 . fst <*< thd3 . pos)
                     <*< ((>=) <&>> thd3 . snd <*< thd3 . pos)

botlines2 :: Nanobot -> [Coord]
botlines2 = liftA3 (,,) <$> (enumFromThenTo <$> ((-) <$> fst3 . pos <*> radius) <*> fst3 . pos <*> ((+) <$> fst3 . pos <*> radius))
                        <*> (enumFromThenTo <$> ((-) <$> snd3 . pos <*> radius) <*> snd3 . pos <*> ((+) <$> snd3 . pos <*> radius))
                        <*> (enumFromThenTo <$> ((-) <$> thd3 . pos <*> radius) <*> thd3 . pos <*> ((+) <$> thd3 . pos <*> radius))

botlines :: Nanobot -> [(Coord,Coord)]
botlines = bar ... filter <$> (/=) . pos <*> (filter <$> quux . pos <*> botlines2)

quux :: Coord -> Coord -> Bool
quux = anyOf <$$>>> ((&&) <$$>> ((==) `on` fst3) <*< ((==) `on` snd3))
                <*< ((&&) <$$>> ((==) `on` fst3) <*< ((==) `on` thd3))
                <*< ((&&) <$$>> ((==) `on` snd3) <*< ((==) `on` thd3))

atLeastOneEqual :: Coord -> Coord -> Bool
atLeastOneEqual = anyOf <$$>>> ((==) `on` fst3) <*< ((==) `on` snd3) <*< ((==) `on` thd3)

bar :: [Coord] -> [(Coord,Coord)]
bar = filter (uncurry atLeastOneEqual) . filter (uncurry (/=)) . (liftA2 (,) <$> id <*> id)

inRangeOf :: (Coord,Coord) -> Nanobot -> Bool
inRangeOf = (||) <$$>> or ... (flip fmap <&>> corners <*< flip inRange)
                   <*< or ... (fmap <&>> intersects <*< botlines)

inRangeOfBots2 :: (Coord,Coord) -> [Nanobot] -> ((Coord,Coord),Int)
inRangeOfBots2 = (.) <$> (,) <*> length ... filter . ((||) <$$>> inside <*< inRangeOf)

getCorners :: Int -> [Nanobot] -> [(Int, Int, Int)]
getCorners = nub ... fmap . (. pos) . both3 . flip div . (^2)

limits :: [Coord] -> (Int, Int)
limits = (minimum &&& maximum) . concatMap (uncurry3 $ listOf <&>>> id <*< id <*< id)

boxify :: Int -> Coord -> Coord -> [(Coord,Coord)]
boxify = (...) <$$$>> nub ... fmap . (id &&&) . (both3 . (+)) ... arg31 <*< booox

booox :: Int -> Coord -> Coord -> [Coord]
booox = liftA3 (,,) <$$$>>> (enumFromThenTo <$$$>>> fst3 ... arg32 <*< ((+) <$$$>> arg31 <*< fst3 ... arg32) <*< pred . fst3 ... arg33)
                        <*< (enumFromThenTo <$$$>>> snd3 ... arg32 <*< ((+) <$$$>> arg31 <*< snd3 ... arg32) <*< pred . snd3 ... arg33)
                        <*< (enumFromThenTo <$$$>>> thd3 ... arg32 <*< ((+) <$$$>> arg31 <*< thd3 ... arg32) <*< pred . thd3 ... arg33)

foo1 :: Int -> [String] -> [(Coord, Coord)]
foo1 = fmap fst . maximumsBy snd ... (. fmap nanobot) . (fmap <$$>> flip ($) ... arg2
                                                     <*< fmap inRangeOfBots2 ... ((.) <$> uncurry . (boxify <&>>> id <*< triple <*< triple) <*> limits ... getCorners) )

foo2 :: (Coord, Coord) -> Int -> [String] -> [(Coord, Coord)]
foo2 = (fmap fst . maximumsBy snd ... (. fmap nanobot)) .* (fmap <$$$>> (. inRangeOfBots2) . flip ($) ... arg33
                                                          <*< (($) <$$$>> uncurry . boxify ... arg32 <*< arg31))

intersects :: (Coord,Coord) -> (Coord,Coord) -> Bool
--intersects box line@(a,b) = intersects_ box line && intersects_ box (b,a)
intersects = (&&) <$$>> intersects_ <*< (intersects_ <&>> id <*< swap)

blah :: Coord -> Coord -> Coord
blah = (,,) <$$>>> ((-) `on` fst3) <*< ((-) `on` snd3) <*< ((-) `on` thd3)

-- uhh, this is a bit too heavy...
intersects_ :: (Coord,Coord) -> (Coord,Coord) -> Bool
intersects_ ((x1,y1,z1),(x2,y2,z2)) aa@((a1,b1,c1),_) = let
    (a2,b2,c2) = both3 fromIntegral $ uncurry blah aa

    tx1 = fromIntegral (x1 - a1) / a2
    tx2 = fromIntegral (x2 - a1) / a2

    ty1 = fromIntegral (y1 - b1) / b2
    ty2 = fromIntegral (y2 - b1) / b2

    tz1 = fromIntegral (z1 - c1) / c2
    tz2 = fromIntegral (z2 - c1) / c2

    tmin3 = max (max (max (-9999999999999999999) (min tx1 tx2)) (min ty1 ty2)) (min tz1 tz2)
    tmax3 = min (min (min   9999999999999999999  (max tx1 tx2)) (max ty1 ty2)) (max tz1 tz2)
  in
    tmax3 >= tmin3 && tmax3 >= 0

foo3 :: Int -> [String] -> (Coord, Coord) -> [(Coord, Coord)]
foo3 = (flip .) . flip $ foo2

qux :: [String] -> [(Coord, Coord)] -> [(Coord, Coord)]
qux = appEndo . mconcat (fmap ((Endo .) . (concatMap .) . foo3) [1,10,100,1000,10000,100000,1000000,10000000])

foo :: [String] -> Coord -> (Int, (Int, Coord))
foo = (,) <$$>> flip inRangeOfBots . fmap nanobot <*< const (manhattan (0,0,0) &&& id)

inp2 :: [String] -> [(Int, (Int, Coord))]
inp2 = fmap <$> ((.) <$> (head . sortOn Down ... fmap . foo)
                     <*> const (((++) `on` singleton) <$> fst <*> snd))
            <*> (($) <$> qux <*> foo1 100000000)

-- What is the shortest manhattan distance between any of those points and 0,0,0?
solution2 = fst . snd . head . inp2 <$> input
-- 105191907
