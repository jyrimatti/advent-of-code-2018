{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
module Day24 where

import           Algorithm.Search (bfs)
import           Control.Arrow ((&&&))
import           Control.Monad ((>=>))
import           Data.Bifunctor (second,first,bimap)
import           Data.Foldable (maximumBy, toList)
import           Data.Function (on)
import           Data.List (delete, group, sort,nub,cycle,iterate',(\\),sortBy,nubBy,groupBy,sortOn,zip)
import           Data.List.Extra (groupSortBy)
import qualified Data.Matrix as M
import           Data.Matrix (Matrix, (!))
import           Data.Maybe (isJust,fromJust,isNothing,listToMaybe,maybeToList,catMaybes,fromMaybe,mapMaybe)
import           Data.Ord (comparing,Down(..))
import qualified Data.Sequence as S
import           Data.Sequence (Seq(..))
import           Data.Tuple (swap)
import           Data.Tuple.Extra (swap,both)
import qualified Data.Vector as V
import           Data.Vector (Vector)
import           Prelude hiding (Either(Left,Right),round)
import           Text.Megaparsec (Parsec,parse,optional,(<|>),try,many)
import           Text.Megaparsec.Char (char,space,string,anyChar,letterChar)
import           Text.Megaparsec.Char.Lexer (decimal)
import Debug.Trace
import Control.Monad.Combinators (between,sepBy,manyTill)

inputLines = fmap lines . readFile 

input = inputLines "input/input24.txt"
test  = inputLines "input/input24_test.txt"

type AttackType = String

data Group = Group {
    armyName :: String,
    units :: Int,
    unitHp :: Int,
    weaknesses :: [AttackType],
    immunities :: [AttackType],
    damage :: Int,
    attackType :: AttackType,
    initiative :: Int
} deriving (Show,Eq)

type Parser = Parsec () String

armyP :: Parser String
armyP = manyTill anyChar (char ':')

weaknessesP :: Parser [AttackType]
weaknessesP = string "weak to " *> (many letterChar `sepBy` string ", ") <* optional (string "; ")

immunitiesP :: Parser [AttackType]
immunitiesP = string "immune to " *> (many letterChar `sepBy` string ", ") <* optional (string "; ")

weaknessOrImmunityP :: Parser ([AttackType],[AttackType])
weaknessOrImmunityP = try ((,[]) <$> weaknessesP) <|> (([],) <$> immunitiesP)

foosP :: Parser ([AttackType],[AttackType])
foosP = both concat . unzip . fromMaybe [([],[])] <$> optional (between (char '(') (string ") ") $ many weaknessOrImmunityP)

groupP :: String -> Parser Group
groupP a = (\u hp (w,i) d at ini -> Group a u hp w i d at ini) <$> (decimal <* string " units each with ") <*> (decimal <* string " hit points ") <*> foosP <*> (string "with an attack that does " *> decimal <* char ' ') <*> (manyTill letterChar (char ' ') <* string "damage at initiative ") <*> decimal



army :: [String] -> [Group]
army lines = let
    name = either undefined id . parse armyP "" . head $ lines
  in
    fmap (either undefined id . parse (groupP name) "") . tail $ lines

effectivePower = (*) <$> units <*> damage

enemyGroups n = filter ((/= n) . armyName)

dealDamage :: Group -> Group -> Int
dealDamage attacking defending | attackType attacking `elem` immunities defending = 0
dealDamage attacking defending | attackType attacking `elem` weaknesses defending = 2 * effectivePower attacking
dealDamage attacking defending                                                    =     effectivePower attacking

allocateTarget :: Group -> [Group] -> Maybe Group
allocateTarget attacking candidateGroups = let
    candidates = filter ((> 0) . dealDamage attacking) $ enemyGroups (armyName attacking) candidateGroups
    target = maximumBy (comparing (dealDamage attacking &&& effectivePower &&& initiative)) candidates
  in
    if null candidates then Nothing else Just target

targetSelectionPhase :: [Group] -> [(Group, Maybe Group)]
targetSelectionPhase allGroups = let 
    groupsInAttackOrder = sortOn (Down . effectivePower &&& initiative) allGroups
    foo (attacking:remaining,done) = (remaining,) $ ((: done) . (attacking,)) $ allocateTarget attacking $ (remaining <> fmap fst done) \\ mapMaybe snd done
    foo ret@([],done) = ret
  in
    snd $ head $ dropWhile (not . null . fst) $ iterate' foo (groupsInAttackOrder,[])

attack :: Group -> Group -> Group
attack attacking defending | units attacking <= 0 = defending
attack attacking defending                        = defending { units = units defending - dealDamage attacking defending `div` unitHp defending }

attackPhase :: [(Group, Maybe Group)] -> [Group]
attackPhase pairs = let
    attackOrder = S.fromList $ sortOn (Down . initiative . fst) pairs
    notBeingAttackedOn = fmap fst pairs \\ mapMaybe snd pairs
  in
    notBeingAttackedOn <> filter ((> 0) . units) (snd (head $ dropWhile (not . null . fst) $ iterate' bar (attackOrder,[])))

bar :: (Seq (Group,Maybe Group), [Group]) -> (Seq (Group,Maybe Group), [Group])
bar (a,defended) | null a = (a, defended)
bar ((a,Nothing) :<| remaining,defended) = (remaining, defended)
bar ((a,Just d) :<| remaining,defended) = let
    defender = attack a d
    toReplace = S.findIndexL ((== d) . fst) remaining
    pendingAttackers | isNothing toReplace = remaining
                     | otherwise           = S.adjust' (first (const defender)) (fromJust toReplace) remaining
  in (pendingAttackers, defender : defended)

step :: [Group] -> [Group]
step = attackPhase . targetSelectionPhase

boost amount = first (fmap (\g -> g { damage = damage g + amount }))

skipWhileAdvancing = fmap snd . dropWhile (uncurry (/=)) . (zip <$> id <*> tail)

onlySingleArmy = (== 1) . length . nub . fmap armyName

solve1 :: Int -> [String] -> [Group]
solve1 boostAmount = concat . take 1 . takeWhile onlySingleArmy . skipWhileAdvancing . iterate' step . uncurry (<>) . boost boostAmount . both army . second tail . span (/= "")

-- how many units would the winning army have?
solution1 = sum . fmap units . solve1 0 <$> input
-- 14000

sameArmy a b = nub (fmap armyName a) == nub (fmap armyName b)

solve2 = sum . fmap units . head . (!! 1) . groupBy sameArmy . filter (not . null) . traverse solve1 [0..]

-- How many units does the immune system have left
solution2 = solve2 <$> input
-- 6149