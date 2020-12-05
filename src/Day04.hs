{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
module Day04 where 
    
import Control.Arrow              ((&&&))
import Control.Conditional (if')
import Control.Lens               (over, makeLenses)
import Data.Function              (on)
import Data.Functor.Identity      (Identity)
import Data.List                  (find, group, groupBy, maximumBy,
                                             nub, sort, sortOn)
import Data.List.Extra            (groupOn, groupSortBy, groupSortOn,
                                             maximumOn)
import Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                , isJust
                                                )
import Data.Profunctor                ( rmap )
import Data.Tuple                 (swap)
import Data.Tuple.Extra           (both, first, second)
import Text.Megaparsec            (Parsec, anySingleBut, many,
                                             optional, parseMaybe, try, (<|>))
import Text.Megaparsec.Char       (char, letterChar, space, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Universum ((...))
import Util



input = lines <$> readFile "input/input04.txt"

data Event = BeginShift | FallAsleep | WakeUp
    deriving (Show, Eq)

type GuardId = Int

data Record = Record {
    _minute :: Int,
    _guard  :: Maybe GuardId,
    _event  :: Event
} deriving Show

makeLenses ''Record

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
withPreviousGuardIfMissing = over guard <&>> flip (<|>) . _guard . head <*< id

withGuardId :: [Record] -> Record -> [Record]
withGuardId = (:) <$$>> withPreviousGuardIfMissing <*< arg1

records :: [String] -> [Record]
records = reverse . foldl withGuardId [] . fmap record . sort

guardsMatch :: Maybe GuardId -> Maybe GuardId -> Bool
guardsMatch = (&&) <$$>> (==) <*< (&&) `oN` isJust -- `on` but with higher fixity

eventsMatch :: Event -> Event -> Bool
eventsMatch = (&&) <&>> (== FallAsleep) <*< (== WakeUp)

recordsMatch :: Record -> Record -> Bool
recordsMatch = (&&) <$$>> guardsMatch `oN` _guard <*< eventsMatch `oN` _event

minutesForGuard :: Record -> Record -> (GuardId, (Int, Int))
minutesForGuard = (,) <$$>> fromJust . _guard ... arg1 <*< (,) `oN` _minute

newElement :: Record -> Record -> [(GuardId, (Int, Int))]
newElement = if' <$$>>> recordsMatch
                    <*< singleton ... minutesForGuard
                    <*< const2 []
-- if' is the familiar 'if' expression, but as a regular function

collectAsleepMinutes :: (Record, Record) -> [(GuardId, (Int, Int))] -> [(GuardId, (Int, Int))]
--collectAsleepMinutes (Record m1 (Just g1) FallAsleep, Record m2 (Just g2) WakeUp) xs | g1 == g2 = (g1,(m1,m2)) : xs
--collectAsleepMinutes _ xs = xs
--collectAsleepMinutes xs = \case (Record m1 (Just g1) FallAsleep, Record m2 (Just g2) WakeUp) | g1 == g2 -> (g1,(m1,m2)) : xs; _ -> xs
collectAsleepMinutes = (<>) <&>> uncurry newElement <*< id

pairwise :: [a] -> [(a, a)]
pairwise = zip <$> id <*> tail

groupByGuardId :: [(GuardId, b)] -> (GuardId, [b])
groupByGuardId = first (head . nub) . unzip

asleepMinutesPerGuard :: [String] -> [(GuardId, [(Int, Int)])]
asleepMinutesPerGuard = fmap groupByGuardId . groupSortOn fst . foldl (flip collectAsleepMinutes) [] . pairwise . records

guardMostAsleep :: [String] -> (GuardId, [(Int, Int)])
guardMostAsleep = maximumOn (sum . fmap (uncurry (-) . swap) . snd) . asleepMinutesPerGuard

mostCommonMinute :: [(Int, Int)] -> [Int]
mostCommonMinute = maximumOn length . group . sort . concatMap (enumFromTo <$> fst <*> pred . snd)

solve1 :: [String] -> Int
solve1 = uncurry (*) . second (head . mostCommonMinute) . guardMostAsleep

-- What is the ID of the guard you chose multiplied by the minute you chose?
solution1 = solve1 <$> input
-- 77084


guardMostFrequentlyAsleep :: [String] -> (GuardId, [Int])
guardMostFrequentlyAsleep = maximumOn (length . snd) . fmap (second mostCommonMinute) . asleepMinutesPerGuard

solve2 :: [String] -> Int
solve2 = uncurry (*) . second head . guardMostFrequentlyAsleep

-- What is the ID of the guard you chose multiplied by the minute you chose?
solution2 = solve2 <$> input
-- 23047