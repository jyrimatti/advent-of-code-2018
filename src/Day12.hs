{-# LANGUAGE TemplateHaskell #-}
module Day12 where

import           Control.Conditional (if')
import           Control.Lens hiding ((...),index,(<|),(|>),transform,both)
import           Data.Foldable                        (toList)
import           Data.FoldApp (listOf)
import           Data.List                            (iterate')
import qualified Data.Map.Strict                      as M
import           Data.Maybe                           (fromJust, fromMaybe)
import qualified Data.Sequence                        as S
import           Data.Sequence                        (Seq ((:<|), (:|>)), (<|), (|>))
import           Data.Tuple.Extra                     (both)
import           Text.Megaparsec            (Parsec, anySingle, many, optional,
                                             parseMaybe, try, (<|>))
import           Text.Megaparsec.Char       (char, letterChar, space, string)
import           Text.Megaparsec.Char.Lexer (decimal, signed)
import           Universum.VarArg ((...))
import           Util

input = lines <$> readFile "input/input12.txt"

type State = Char

data Pot = Pot {
    _num :: Int,
    _state :: State
} deriving (Show, Eq)

makeLenses ''Pot

type Parser = Parsec () String

potP :: Parser Char
potP = char '.' <|> char '#'

initialStateP :: Parser String
initialStateP = string "initial state: " *> many potP

initialState :: String -> [Pot]
initialState = fmap (uncurry Pot) . zip [0..] . fromJust . parseMaybe initialStateP

data Note = Note {
    ll :: State,
    l :: State,
    cur :: State,
    r :: State,
    rr :: State,
    target :: State
} deriving Show

noteP :: Parser Note
noteP = Note <$> potP <*> potP <*> potP <*> potP <*> potP <*> (string " => " *> potP)

note :: String -> Note
note = fromJust . parseMaybe noteP

valueOfFirst :: Seq Pot -> Int
valueOfFirst = _num . fromJust . S.lookup 0

valueOfLast :: Seq Pot -> Int
valueOfLast = _num . fromJust . (S.lookup <$> pred . length <*> id)

firstStates :: Int -> Seq Pot -> [State]
firstStates = fmap _state . toList ... S.take

lastStates :: Int -> Seq Pot -> [State]
lastStates = fmap _state . toList ... (S.drop <$$>> (flip (-) <&>> id <*< S.length) <*< arg2)

extendLeft :: Seq Pot -> Seq Pot
--extendLeft pots@(Pot _ '.' :<| Pot _ '.' :<| Pot _ '.' :<| Pot _ '.' :<| ps)  = extendLeft ps
--extendLeft pots@(Pot _ '.' :<| Pot _ '.' :<| Pot _ '.' :<| _)                 = pots
--extendLeft pots                                                               = extendLeft $ Pot (valueOfFirst pots - 1) '.' S.<| pots
extendLeft = if' <$> (== "....") . firstStates 4
                 <*> extendLeft . S.drop 4 <*> (
             if' <$> (== "...") . firstStates 3
                 <*> id
                 <*> extendLeft . ((<|) <$> flip Pot '.' . pred . valueOfFirst <*> id) )

extendRight :: Seq Pot -> Seq Pot
extendRight = if' <$> (== "....") . lastStates 4
                  <*> extendRight . (S.take <$> flip (-) 4 . S.length <*> id) <*> (
              if' <$> (== "...") . lastStates 3
                  <*> id
                  <*> extendRight . ((|>) <$> id <*> flip Pot '.' . succ . valueOfLast) )

extend :: Seq Pot -> Seq Pot
extend = extendRight . extendLeft

findTarget :: M.Map [State] Char -> [State] -> Char
--findTarget notes key = fromMaybe '.' $ notes M.!? key
findTarget = fromMaybe '.' ... (M.!?)

lookupOffset :: (Int -> Int) -> Seq Pot -> Int -> Pot
lookupOffset f = fromJust ... (S.lookup <$$>> f ... arg2 <*< const)

mkList5 :: a -> a -> a -> a -> a -> [a]
mkList5 = listOf

transform :: Seq Pot -> Int -> M.Map [State] Char -> Pot
{-transform pots index _ | index <= 1 || index >= length pots - 2 = fromJust $ S.lookup index pots
transform pots index notes = let
    (Pot _ ll1)    = fromJust $ S.lookup (index-2) pots
    (Pot _ l1)     = fromJust $ S.lookup (index-1) pots
    p@(Pot _ cur1) = fromJust $ S.lookup index pots
    (Pot _ r1)     = fromJust $ S.lookup (index+1) pots
    (Pot _ rr1)    = fromJust $ S.lookup (index+2) pots
  in
    p { state = findTarget notes [ll1,l1,cur1,r1,rr1] }
-}
transform = if' <$$>>> ( (||) <$$>> (<= 1) ... arg2
                                <*< ((<=) <&>> pred . pred . length <*< id) )
                   <*< const ... lookupOffset id
                   <*< (flip . flip (set state ... findTarget) ... mkList5 <$$>>>>>> -- oh god...
                                   _state ... lookupOffset (pred . pred)
                               <*< _state ... lookupOffset pred
                               <*< _state ... lookupOffset id
                               <*< _state ... lookupOffset succ
                               <*< _state ... lookupOffset (succ . succ)
                               <*< lookupOffset id)

mapToIndex :: Seq a -> Seq Int
mapToIndex = S.fromList . enumFromTo 0 . pred . length

transformAll :: M.Map [State] Char -> Seq Pot -> Seq Pot
--transformAll notes = uncurry fmap . ((\a b -> transform a b notes) &&& mapToIndex) . extend
transformAll = fmap <$$>> flip (flip . transform . extend) <*< mapToIndex . extend ... arg2 -- aargh...

calculateSum :: Seq Pot -> Int
calculateSum = sum  . fmap _num . S.filter ((== '#') . _state)

pairWithNext :: [a] -> [(a, a)]
pairWithNext = zip <$> id <*> tail

takeUntilDuplicate :: [Seq Pot] -> [Seq Pot]
takeUntilDuplicate = fmap snd . takeWhile (uncurry (/=) . both (fmap _state)) . pairWithNext

remainingGenerations :: Int -> [Seq Pot] -> Int
remainingGenerations = (-) <&>> id <*< length

occupiedPots :: Seq Pot -> Seq Pot
occupiedPots = S.filter ((== '#') . _state)

amountToAddToMissingGenerations :: [Seq Pot] -> Int
amountToAddToMissingGenerations = length . occupiedPots . last

getSum :: Int -> [Seq Pot] -> Int
--getSum generations xs | null (drop generations xs) = let
--    remainingGenerations = generations - length xs
--    occupiedPots = S.filter ((== '#') . _state)
--    lastGeneration = last xs
--    amountToAddToMissingGenerations = length $ occupiedPots lastGeneration
--  in calculateSum lastGeneration + (remainingGenerations * amountToAddToMissingGenerations)
--getSum generations xs = calculateSum (xs !! (generations-1))
getSum = if' <$$>>> null ... drop 
                <*< ( (+) <$$>> calculateSum . last ... arg2
                            <*< ((*) <$$>> remainingGenerations <*< amountToAddToMissingGenerations ... arg2) )
                <*< calculateSum ... (flip (!!) <&>> pred <*< id)

initState :: [String] -> Seq Pot
initState = S.fromList . initialState . head

notes :: [String] -> M.Map [State] State
notes = M.fromList . fmap ( ((,) <$> (listOf <$> ll <*> l <*> cur <*> r <*> rr) <*> target) . note) . drop 2

solve :: Int -> [String] -> Int
solve = getSum <&>> id
                <*< takeUntilDuplicate . (iterate' <$> transformAll . notes <*> initState)

-- After 20 generations, what is the sum of the numbers of all pots which contain a plant?
solution1 = solve 20 <$> input
-- 2917

-- After fifty billion (50000000000) generations, what is the sum of the numbers of all pots which contain a plant?
solution2 = solve 50000000000 <$> input
-- 3250000000956