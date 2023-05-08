{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Day04 where

import           GHC.Generics (Generic)
import           Control.Applicative.Combinators (many, (<|>))
import           Control.Conditional (if')
import           Control.Lens (over)
import           Data.Bifunctor (first, second)
import           Data.Function (on)
import           Data.Generics.Product (field)
import           Data.List (group, sort, singleton)
import           Data.List.Extra (groupSortOn, maximumOn, nubOrd)
import           Data.Maybe (fromJust, isJust)
import           Data.Tuple (swap)
import           Text.Megaparsec (Parsec, anySingleBut, parseMaybe)
import           Text.Megaparsec.Char (char, string)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Universum ((...))
import           Util ((<$$>>), (<$$>>>), (<&>>), (<*<), const2, (<&), (&>))


input :: IO [String]
input = lines <$> readFile "input/input04.txt"

data Event = BeginShift | FallAsleep | WakeUp
    deriving (Show, Eq)

type GuardId = Int

data Record = Record {
    minute :: Int,
    guard  :: Maybe GuardId,
    event  :: Event
} deriving (Show,Generic)

type Parser = Parsec () String

recordP :: Parser Record
recordP = uncurry . Record <$> (many (anySingleBut ':') *> char ':' *> decimal)
                           <*> (string "] " *> eventP)

eventP :: Parser (Maybe GuardId, Event)
eventP = ( (,BeginShift) . Just <$> (string "Guard #" *> decimal <* string " begins shift") ) <|>
         ( (Nothing,FallAsleep) <$ string "falls asleep" ) <|>
         ( (Nothing,WakeUp)     <$ string "wakes up" )

record :: String -> Record
record = fromJust . parseMaybe recordP

withPreviousGuardIfMissing :: [Record] -> Record -> Record
withPreviousGuardIfMissing = flip (<|>) . guard . head &> over (field @"guard") <& id

withGuardId :: [Record] -> Record -> [Record]
withGuardId = (:) <$$>> withPreviousGuardIfMissing <*< const

records :: [String] -> [Record]
records = reverse . foldl withGuardId [] . fmap record . sort

guardsMatch :: Maybe GuardId -> Maybe GuardId -> Bool
guardsMatch = (&&) <$$>> (==) <*< ((&&) `on` isJust) -- guardId == guardId AND both are defined

eventsMatch :: Event -> Event -> Bool
eventsMatch = (== FallAsleep) &> (&&) <& (== WakeUp) -- first arg is FallAsleep AND second arg is WakeUp

recordsMatch :: Record -> Record -> Bool
recordsMatch = (&&) <$$>> (guardsMatch `on` guard) <*< (eventsMatch `on` event) -- both match by guard AND both match by event

minutesForGuard :: Record -> Record -> (GuardId, (Int, Int))
minutesForGuard   = (,) <$$>> fromJust . guard ... const <*< ((,) `on` minute)
--minutesForGuard = (,) <$$>> fromJust . guard ... const <*< both minute ... (,)
--minutesForGuard = (,) <$$>> fromJust . guard ... const <*< curry (both minute)

newElement :: Record -> Record -> [(GuardId, (Int, Int))]
newElement = if' <$$>>> recordsMatch
                    <*< singleton ... minutesForGuard
                    <*< const2 []
-- if' is the familiar 'if' expression, but as a regular function

collectAsleepMinutes :: (Record, Record) -> [(GuardId, (Int, Int))] -> [(GuardId, (Int, Int))]
--collectAsleepMinutes (Record m1 (Just g1) FallAsleep, Record m2 (Just g2) WakeUp) xs | g1 == g2 = (g1,(m1,m2)) : xs
--collectAsleepMinutes _ xs = xs
--collectAsleepMinutes xs = \case (Record m1 (Just g1) FallAsleep, Record m2 (Just g2) WakeUp) | g1 == g2 -> (g1,(m1,m2)) : xs; _ -> xs
collectAsleepMinutes = uncurry newElement &> (<>) <& id

pairwise :: [a] -> [(a, a)]
pairwise = zip <$> id <*> tail

groupByGuardId :: [(GuardId, b)] -> (GuardId, [b])
groupByGuardId = first (head . nubOrd) . unzip

asleepMinutesPerGuard :: [String] -> [(GuardId, [(Int, Int)])]
asleepMinutesPerGuard = fmap groupByGuardId . groupSortOn fst . foldl (flip collectAsleepMinutes) [] . pairwise . records

guardMostAsleep :: [String] -> (GuardId, [(Int, Int)])
guardMostAsleep = maximumOn (sum . fmap (uncurry (-) . swap) . snd) . asleepMinutesPerGuard

mostCommonMinute :: [(Int, Int)] -> [Int]
mostCommonMinute = maximumOn length . group . sort . concatMap (enumFromTo <$> fst <*> pred . snd)

solve1 :: [String] -> Int
solve1 = uncurry (*) . second (head . mostCommonMinute) . guardMostAsleep

-- What is the ID of the guard you chose multiplied by the minute you chose?
solution1 :: IO Int
solution1 = solve1 <$> input
-- 77084


guardMostFrequentlyAsleep :: [String] -> (GuardId, [Int])
guardMostFrequentlyAsleep = maximumOn (length . snd) . fmap (second mostCommonMinute) . asleepMinutesPerGuard

solve2 :: [String] -> Int
solve2 = uncurry (*) . second head . guardMostFrequentlyAsleep

-- What is the ID of the guard you chose multiplied by the minute you chose?
solution2 :: IO Int
solution2 = solve2 <$> input
-- 23047