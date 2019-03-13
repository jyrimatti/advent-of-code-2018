module Day04 where 

import Text.Parsec (ParsecT,parse,many,many1,optional,(<|>))
import Text.Parsec.Char (char,space,string,letter,digit,anyChar,noneOf)
import Text.Parsec.Combinator (between,sepBy)
import Text.ParserCombinators.Parsec.Number (int)
import Data.Functor.Identity (Identity)
import Data.Maybe (fromMaybe, fromJust)
import Control.Arrow ((&&&))
import Data.List (sort,groupBy, maximumBy, find, group, sortOn, nub)
import Data.List.Extra (groupSortBy)
import Data.Function (on)
import Data.Tuple (swap)
import Data.Tuple.Extra (first,second,both)

input = lines <$> readFile "input/input04.txt"

data Event = BeginShift | FallAsleep | WakeUp deriving Show

data Record = Record {
    _minute :: Int,
    _guard :: Maybe Int,
    _event :: Event
} deriving Show

recordP :: ParsecT String u Identity Record
recordP = (\a (b,c) -> Record a b c) <$> ((many (noneOf ":") *> char ':') *> int) <*> (string "] " *> eventP)

eventP = ((\i -> (Just i, BeginShift)) <$> (string "Guard #" *> int <* string " begins shift")) <|>
         (const (Nothing,FallAsleep)   <$> string "falls asleep") <|>
         (const (Nothing,WakeUp)       <$> string "wakes up")

record = either undefined id . parse recordP ""

populateMissingGuardIds acc@(Record _ (Just g) _ : _) (Record a b c) = Record a (Just (fromMaybe g b)) c : acc
populateMissingGuardIds [] r = [r]

records = reverse . foldl populateMissingGuardIds [] . fmap record . sort

collectAsleepMinutes xs (Record m1 (Just g1) FallAsleep, Record m2 (Just g2) WakeUp    ) | g1 == g2 = (g1,(m1,m2)) : xs
--collectAsleepMinutes xs (Record _  (Just g1) WakeUp    , Record _  (Just g2) FallAsleep) | g1 == g2 = xs
--collectAsleepMinutes xs (Record _  (Just g1) BeginShift, Record _  (Just g2) FallAsleep) | g1 == g2 = xs
--collectAsleepMinutes xs (Record _  (Just g1) WakeUp    , Record _  (Just g2) BeginShift)            = xs
--collectAsleepMinutes xs (Record _  (Just g1) BeginShift, Record _  (Just g2) BeginShift) | g1 /= g2 = xs
collectAsleepMinutes xs _ = xs

asleepMinutes = fmap (first (head . nub) . unzip) . groupSortBy (compare `on` fst) . foldl collectAsleepMinutes [] . uncurry zip . (id &&& tail) . records

guardMostAsleep = maximumBy (compare `on` sum . fmap (uncurry (-) . swap) . snd) . asleepMinutes

mostCommonMinute = head . last . sortOn length . group . sort . concatMap (\(a,b) -> [a..b-1])

solve1 = uncurry (*) . second mostCommonMinute . guardMostAsleep

guardMostFrequentlyAsleep = maximumBy (compare `on` length . snd) . fmap (second (maximumBy (compare `on` length) . group . sort . concatMap (\(a,b) -> [a..b-1]))) . asleepMinutes

solve2 = uncurry (*) . second head . guardMostFrequentlyAsleep

-- What is the ID of the guard you chose multiplied by the minute you chose?
solution1 = solve1 <$> input
-- 77084

-- What is the ID of the guard you chose multiplied by the minute you chose?
solution2 = solve2 <$> input
-- 23047