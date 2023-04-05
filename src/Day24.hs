{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
module Day24 where

import           Algorithm.Search (bfs)
import           Control.Arrow ((&&&))
import           Control.Monad ((>=>))
import           Control.Monad.Combinators (between, manyTill, sepBy)
import           Data.Bifunctor (bimap, first, second, Bifunctor)
import           Data.Foldable (maximumBy, toList)
import           Data.Function (on)
import           Data.List (cycle, delete, group, groupBy, iterate', nub, nubBy, sort, sortBy, sortOn, zip, (\\))
import           Data.List.Extra (groupSortBy)
import qualified Data.Matrix as M
import           Data.Matrix (Matrix, (!))
import           Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import           Data.Ord (Down(..), comparing)
import qualified Data.Sequence as S
import           Data.Sequence (Seq(..))
import           Data.Tuple (swap)
import           Data.Tuple.Extra (both, swap)
import qualified Data.Vector as V
import           Data.Vector (Vector)
import           Debug.Trace ()
import           Prelude hiding (Either(Left, Right), round)
import           Text.Megaparsec (Parsec, anySingle, many, optional, parse, try, (<|>))
import           Text.Megaparsec.Char (char, letterChar, space, string)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Universum ((...), over, Generic)
import           Data.Composition ((.**), (.***), (.*), (.****), compose2)
import           Util ((<&), (&>), (<$$>>>>), (<*<), const2, (<$$>>>), (<$$>>), (<&>>), arg2, arg1, (<$$$>>), arg32, arg33)
import           Control.Conditional (if')
import           Data.Generics.Product (setField, HasField(..))

inputLines :: FilePath -> IO [String]
inputLines = fmap lines . readFile 

input, test :: IO [String]
input = inputLines "input/input24.txt"
test  = inputLines "input/input24_test.txt"

type AttackType = String

data Group = Group {
    units      :: Int,
    unitHp     :: Int,
    weaknesses :: [AttackType],
    immunities :: [AttackType],
    damage     :: Int,
    attackType :: AttackType,
    initiative :: Int,
    armyName   :: String
} deriving (Show,Eq,Generic)

type Parser = Parsec () String

armyP :: Parser String
armyP = manyTill anySingle (char ':')

weaknessesP :: Parser [AttackType]
weaknessesP = string "weak to " *> (many letterChar `sepBy` string ", ") <* optional (string "; ")

immunitiesP :: Parser [AttackType]
immunitiesP = string "immune to " *> (many letterChar `sepBy` string ", ") <* optional (string "; ")

weaknessOrImmunityP :: Parser ([AttackType],[AttackType])
weaknessOrImmunityP = try ((,[]) <$> weaknessesP) <|> (([],) <$> immunitiesP)

weaknessesAndImmunitiesP :: Parser ([AttackType],[AttackType])
weaknessesAndImmunitiesP = both concat . unzip . fromMaybe [([],[])] <$> optional (between (char '(') (string ") ") $ many weaknessOrImmunityP)

groupP :: Parser (String -> Group)
groupP = id .**** uncurry .* Group <$> (decimal <* string " units each with ")
                                   <*> (decimal <* string " hit points ")
                                   <*> weaknessesAndImmunitiesP
                                   <*> (string "with an attack that does " *> decimal <* char ' ')
                                   <*> (manyTill letterChar (char ' ') <* string "damage at initiative ")
                                   <*> decimal

army :: [String] -> [Group]
--army lines = let
--    name = either undefined id . parse armyP "" . head $ lines
--  in
--    fmap (either undefined ($ name) . parse (groupP) "") . tail $ lines
army = fmap . flip ($) <$> either undefined id . parse armyP "" . head
                       <*> fmap (either undefined id . parse groupP "") . tail

effectivePower :: Group -> Int
effectivePower = (*) <$> units <*> damage

enemyGroups :: String -> [Group] -> [Group]
--enemyGroups n = filter ((/= n) . armyName)
enemyGroups = filter . (. armyName) . (/=)

dealDamage :: Group -> Group -> Int
--dealDamage attacking defending | attackType attacking `elem` immunities defending = 0
--dealDamage attacking defending | attackType attacking `elem` weaknesses defending = 2 * effectivePower attacking
--dealDamage attacking defending                                                    =     effectivePower attacking
dealDamage = if' <$$>>> attackType &> elem <& immunities
                    <*< const2 0 $
             if' <$$>>> attackType &> elem <& weaknesses
                    <*< effectivePower &> (*) <& const 2 $
                        effectivePower ... const

allocateTarget :: Group -> [Group] -> Maybe Group
--allocateTarget attacking candidateGroups = let
--    candidates = filter ((> 0) . dealDamage attacking) $ enemyGroups (armyName attacking) candidateGroups
--  in
--    if null candidates then Nothing else Just (maximumBy (comparing (dealDamage attacking &&& effectivePower &&& initiative)) candidates)
allocateTarget = allocateTarget_ <$$>> const
                                   <*< (filter <$$>> ((> 0) ... dealDamage) ... const
                                                 <*< armyName &> enemyGroups <& id)

allocateTarget_ :: Group -> [Group] -> Maybe Group
allocateTarget_ = if' <$$>>> null ... arg2
                         <*< const2 Nothing $
                             comparing . (&&& effectivePower &&& initiative) . dealDamage &> Just ... maximumBy <& id

foo :: [Group] -> [(Group, Maybe Group)] -> ([Group], [(Group, Maybe Group)])
--foo (attacking:remaining,done) = (remaining,) $ ((: done) . (attacking,)) $ allocateTarget attacking $ (remaining <> fmap fst done) \\ mapMaybe snd done
--foo ret@([],done) = ret
foo = if' <$$>>> null ... arg1
             <*< (,) $
                 ( (,) <$$>> tail ... arg1
                         <*< (($) <$$>> flip (:) ... arg2
                                    <*< ((,) <$$>> head ... arg1
                                               <*< ( allocateTarget <$$>> head ... arg1
                                                                      <*< ( (\\) <$$>> (tail &> (<>) <& fmap fst)
                                                                                   <*< mapMaybe snd ... arg2)))))

targetSelectionPhase :: [Group] -> [(Group, Maybe Group)]
--targetSelectionPhase allGroups = let 
--    groupsInAttackOrder = sortOn (Down . effectivePower &&& initiative) allGroups
--  in
--    snd $ head $ dropWhile (not . null . fst) $ iterate' foo (groupsInAttackOrder,[])
targetSelectionPhase = snd . head . dropWhile (not . null . fst) . iterate' (uncurry foo) . (,[]) . sortOn (Down . effectivePower &&& initiative)

attack :: Group -> Group -> Group
--attack attacking defending | units attacking <= 0 = defending
--attack attacking defending                        = defending { units = units defending - dealDamage attacking defending `div` unitHp defending }
attack = if' <$$>>> (<= 0) . units ... arg1
                <*< arg2 $
                    (over (field @"units") <$$>> ((-) <$$$>> arg33
                                                         <*< (div <$$$>> const ... dealDamage
                                                                     <*< unitHp ... arg32))
                                             <*< arg2)

attackPhase :: [(Group, Maybe Group)] -> [Group]
--attackPhase pairs = let
--    attackOrder = S.fromList $ sortOn (Down . initiative . fst) pairs
--    notBeingAttackedOn = fmap fst pairs \\ mapMaybe snd pairs
--  in
--    notBeingAttackedOn <> filter ((> 0) . units) (snd (head $ dropWhile (not . null . fst) $ iterate' bar (attackOrder,[])))
attackPhase = (<>) <$> ((\\) <$> fmap fst <*> mapMaybe snd)
                   <*> filter ((> 0) . units) . snd . head . dropWhile (not . null . fst) . iterate' (uncurry bar) . (,[]) . S.fromList . sortOn (Down . initiative . fst)

-- first element of a Seq without pattern matching
seqHead :: S.Seq a -> a
seqHead = fromJust ... S.lookup 0

seqTails :: Seq a -> Seq a
seqTails = head . tail . toList . S.tails

bar :: Seq (Group,Maybe Group) -> [Group] -> (Seq (Group,Maybe Group), [Group])
--bar (a,defended) | null a = (a, defended)
--bar ((a,Nothing) :<| remaining,defended) = (remaining, defended)
--bar ((a,Just d) :<| remaining,defended) = let
--    defender = attack a d
--    toReplace = S.findIndexL ((== d) . fst) remaining
--    pendingAttackers | isNothing toReplace = remaining
--                     | otherwise           = S.adjust' (first (const defender)) (fromJust toReplace) remaining
--  in (pendingAttackers, defender : defended)
bar = if' <$$>>> null ... arg1
             <*< (,) $
      if' <$$>>> isNothing . snd . seqHead ... arg1
             <*< seqTails &> (,) <& id $
                 ((,) <$$>> (if' <$$>>> isNothing ... (S.findIndexL . (. fst) <$> (==) . fromJust . snd . seqHead <*> seqTails) ... arg1
                                    <*< seqTails ... arg1 $
                                        (S.adjust' <$$>>> first . const . (attack <$> fst <*> fromJust . snd) . seqHead ... arg1
                                                      <*< fromJust . (S.findIndexL . (. fst) <$> (==) . fromJust . snd . seqHead <*> seqTails) ... arg1
                                                      <*< seqTails ... arg1))
                        <*< (attack <$> fst <*> fromJust . snd) . seqHead &> (:) <& id)

step :: [Group] -> [Group]
step = attackPhase . targetSelectionPhase

boost :: Int -> ([Group], c) -> ([Group], c)
--boost amount = first (fmap (\g -> g { damage = damage g + amount }))
boost = first . fmap . over (field @"damage") . (+)

skipWhileAdvancing :: [[Group]] -> [[Group]]
skipWhileAdvancing = fmap snd . dropWhile (uncurry (/=)) . (zip <$> id <*> tail)

onlySingleArmy :: [Group] -> Bool
onlySingleArmy = (== 1) . length . nub . fmap armyName

solve1 :: Int -> [String] -> [Group]
solve1 = concat . take 1 . takeWhile onlySingleArmy . skipWhileAdvancing . iterate' step . uncurry (<>) ... (. both army . second tail . span (/= "")) . boost

-- how many units would the winning army have?
solution1 :: IO Int
solution1 = sum . fmap units . solve1 0 <$> input
-- 14000

sameArmy :: [Group] -> [Group] -> Bool
--sameArmy a b = nub (fmap armyName a) == nub (fmap armyName b)
sameArmy = (==) `on` nub . fmap armyName

solve2 :: [String] -> Int
solve2 = sum . fmap units . head . (!! 1) . groupBy sameArmy . filter (not . null) . traverse solve1 [0..]

-- How many units does the immune system have left
solution2 :: IO Int
solution2 = solve2 <$> input
-- 6149