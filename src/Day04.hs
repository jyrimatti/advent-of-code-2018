{-# LANGUAGE TupleSections #-}
module Day04 where 

import Control.Arrow ((&&&))
import Data.Function (on)
import Data.Functor.Identity (Identity)
import Data.List (sort,groupBy, maximumBy, find, group, sortOn, nub)
import Data.List.Extra (groupOn, groupSortBy, groupSortOn, maximumOn)
import Data.Maybe (fromMaybe, fromJust)
import Data.Tuple (swap)
import Data.Tuple.Extra (first,second,both)
import Text.Megaparsec (Parsec,parse,optional,(<|>),try,many)
import Text.Megaparsec.Char (char,space,string,anyChar,letterChar,notChar)
import Text.Megaparsec.Char.Lexer (decimal,signed)

input = lines <$> readFile "input/input04.txt"

data Event = BeginShift | FallAsleep | WakeUp
    deriving Show

type GuardId = Int

data Record = Record {
    minute :: Int,
    guard  :: Maybe GuardId,
    event  :: Event
} deriving Show

type Parser = Parsec () String

recordP :: Parser Record
recordP = uncurry . Record <$> ((many (notChar ':') *> char ':') *> decimal) <*> (string "] " *> eventP)

eventP :: Parser (Maybe GuardId, Event)
eventP = ( (,BeginShift) . Just <$> (string "Guard #" *> decimal <* string " begins shift") ) <|>
         ( (Nothing,FallAsleep) <$ string "falls asleep" ) <|>
         ( (Nothing,WakeUp)     <$ string "wakes up" )

record :: String -> Record
record = either undefined id . parse recordP ""

populateMissingGuardIds :: [Record] -> Record -> [Record]
populateMissingGuardIds acc cur = cur { guard = guard cur <|> guard (head acc) } : acc

records :: [String] -> [Record]
records = reverse . foldl populateMissingGuardIds [] . fmap record . sort

collectAsleepMinutes :: [(GuardId, (Int, Int))] -> (Record, Record) -> [(GuardId, (Int, Int))]
collectAsleepMinutes xs (Record m1 (Just g1) FallAsleep, Record m2 (Just g2) WakeUp) | g1 == g2 = (g1,(m1,m2)) : xs
collectAsleepMinutes xs _ = xs

pairwise :: [a] -> [(a, a)]
pairwise = zip <$> id <*> tail

groupByGuardId :: [(GuardId, b)] -> (GuardId, [b])
groupByGuardId = first (head . nub) . unzip

asleepMinutes :: [String] -> [(GuardId, [(Int, Int)])]
asleepMinutes = fmap groupByGuardId . groupSortOn fst . foldl collectAsleepMinutes [] . pairwise . records

guardMostAsleep :: [String] -> (GuardId, [(Int, Int)])
guardMostAsleep = maximumOn (sum . fmap (uncurry (-) . swap) . snd) . asleepMinutes

mostCommonMinute :: [(Int, Int)] -> [Int]
mostCommonMinute = maximumOn length . group . sort . concatMap (\(a,b) -> [a..b-1])

solve1 :: [String] -> Int
solve1 = uncurry (*) . second (head . mostCommonMinute) . guardMostAsleep

-- What is the ID of the guard you chose multiplied by the minute you chose?
solution1 = solve1 <$> input
-- 77084


guardMostFrequentlyAsleep :: [String] -> (GuardId, [Int])
guardMostFrequentlyAsleep = maximumOn (length . snd) . fmap (second mostCommonMinute) . asleepMinutes

solve2 :: [String] -> Int
solve2 = uncurry (*) . second head . guardMostFrequentlyAsleep

-- What is the ID of the guard you chose multiplied by the minute you chose?
solution2 = solve2 <$> input
-- 23047