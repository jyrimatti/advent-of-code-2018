module Day12 where

import           Control.Arrow                        ((&&&))
import           Data.Foldable                        (toList)
import           Data.List                            (iterate')
import qualified Data.Map.Strict                      as M
import           Data.Maybe                           (fromJust, fromMaybe)
import qualified Data.Sequence                        as S
import           Data.Sequence                        (Seq ((:<|), (:|>)), (<|), (|>))
import           Data.Tuple.Extra                     (both)
import           Text.Parsec                          (many, many1, optional,
                                                       parse, (<|>))
import           Text.Parsec.Char                     (anyChar, char, digit,
                                                       letter, space, string)
import           Text.Parsec.Combinator               (between, sepBy)
import           Text.ParserCombinators.Parsec.Number (int)

input = lines <$> readFile "input/input12.txt"

type State = Char

data Pot = Pot {
    num :: Int,
    state :: State
} deriving (Show, Eq)

potP = char '.' <|> char '#'

initialStateP = string "initial state: " *> many potP

initialState = fmap (uncurry Pot) . zip [0..] . either undefined id . parse initialStateP ""

data Note = Note {
    ll :: State,
    l :: State,
    cur :: State,
    r :: State,
    rr :: State,
    target :: State
} deriving Show

noteP = Note <$> potP <*> potP <*> potP <*> potP <*> potP <*> (string " => " *> potP)

note = either undefined id . parse noteP ""

valueOfFirst pots = num (fromJust $ S.lookup 0 pots)
valueOfLast pots = num (fromJust $ S.lookup (length pots - 1) pots)

extendLeft pots@(Pot _ '.' :<| Pot _ '.' :<| Pot _ '.' :<| Pot _ '.' :<| ps)  = extendLeft ps
extendLeft pots@(Pot _ '.' :<| Pot _ '.' :<| Pot _ '.' :<| _)                 = pots
extendLeft pots                                                               = extendLeft $ Pot (valueOfFirst pots - 1) '.' S.<| pots

extendRight pots@(ps :|> Pot _ '.' :|> Pot _ '.' :|> Pot _ '.' :|> Pot _ '.') = extendRight ps
extendRight pots@(_ :|> Pot _ '.' :|> Pot _ '.' :|> Pot _ '.')                = pots
extendRight pots                                                              = extendRight $ pots S.|> Pot (valueOfLast pots + 1) '.'

extend = extendRight . extendLeft

findTarget notes key = fromMaybe '.' $ notes M.!? key

transform notes pots index | index <= 1 || index >= length pots - 2 = fromJust $ S.lookup index pots
transform notes pots index = let
    (Pot _ ll1)    = fromJust $ S.lookup (index-2) pots
    (Pot _ l1)     = fromJust $ S.lookup (index-1) pots
    p@(Pot _ cur1) = fromJust $ S.lookup index pots
    (Pot _ r1)     = fromJust $ S.lookup (index+1) pots
    (Pot _ rr1)    = fromJust $ S.lookup (index+2) pots
  in
    p { state = findTarget notes [ll1,l1,cur1,r1,rr1] }

mapToIndex = (\l -> S.fromList [0..l-1]) . length

transformAll notes = uncurry fmap . (transform notes &&& mapToIndex) . extend

calculateSum = sum . fmap num . S.filter ((== '#') . state)

pairWithNext = uncurry zip . (id &&& tail)

takeUntilDuplicate = fmap snd . takeWhile (uncurry (/=) . both (fmap state)) . pairWithNext

getSum generations xs | null (drop generations xs) = let
    remainingGenerations = generations - length xs
    occupiedPots = S.filter ((== '#') . state)
    lastGeneration = last xs
    amountToAddToMissingGenerations = length $ occupiedPots lastGeneration
  in calculateSum lastGeneration + (remainingGenerations * amountToAddToMissingGenerations)
getSum generations xs = calculateSum (xs !! (generations-1))

initState = S.fromList . initialState . head

notes = M.fromList . fmap ((\(Note ll l cur r rr target) -> ([ll,l,cur,r,rr], target)) . note) . drop 2

solve generations = getSum generations . takeUntilDuplicate . uncurry iterate' . (transformAll . notes &&& initState)

-- After 20 generations, what is the sum of the numbers of all pots which contain a plant?
solution1 = solve 20 <$> input
-- 2917

-- After fifty billion (50000000000) generations, what is the sum of the numbers of all pots which contain a plant?
solution2 = solve 50000000000 <$> input
-- 3250000000956