{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
module Day13 where

import           Control.Arrow ((&&&))
import           Control.Conditional (if')
import           Control.Lens (over, set, makeLenses)
import           Data.Bifunctor (second)
import           Data.FoldApp (allOf)
import           Data.Function (on)
import           Data.List (cycle, groupBy, iterate', nub, nubBy, sort, sortBy, (\\), sortOn)
import           Data.List.Extra (groupOn)
import qualified Data.Matrix as M
import           Data.Matrix (Matrix, (!))
import           Data.Maybe (catMaybes, fromJust, isJust, isNothing, listToMaybe)
import           Data.Tuple (swap)
import qualified Data.Vector as V
import           Data.Vector (Vector)
import           Prelude hiding (Either(Left, Right))
import           Universum.VarArg ((...))
import           Util ((<$$$$$>>), (<$$$$$>>>), (<$$$$>>), (<$$$>>), (<$$$>>>)
                     , (<$$>>), (<$$>>>), (<&>>), (<&>>>), (<&>>>>), (<*<)
                     , anyOf, arg2, arg31, arg32, arg33, arg41, arg42, arg43
                     , arg44, arg51, arg52, arg53, arg54, arg55, compose3
                     , const2, singleton, (<&), (&>))


input :: IO [String]
input = lines <$> readFile "input/input13.txt"

isCart :: Char -> Bool
--isCart '<' = True
--isCart '>' = True
--isCart '^' = True
--isCart 'v' = True
--isCart _ = False
isCart = anyOf <$> (== '<') <*> (== '>') <*> (== '^') <*> (== 'v')

data Move = Left | Straight | Right
    deriving (Show, Eq)

type Coordinate = (Int,Int)

data Cart = Cart {
    _prevTurn  :: Move,
    _direction :: Char,
    _loc       :: Coordinate
} deriving (Show, Eq)

makeLenses ''Cart

type Map = Matrix Char
type Carts = Vector Cart

foo :: Coordinate -> Char -> Maybe (Char, Coordinate)
foo = if' <$$>>> isCart ... arg2
             <*< Just . swap ... (,)
             <*< const2 Nothing

carts :: Map -> Carts
carts = V.fromList . fmap (uncurry (Cart Right)) . catMaybes . concat . M.toLists . M.imap foo

move :: Coordinate -> Coordinate -> Coordinate
--move (r,c) (dr,dc) = (r + dr,c + dc)
move = (,) <$$>> ((+) `on` fst) <*< ((+) `on` snd)

-- keep collided as-is
action :: Cart -> Char -> Cart

{-
action c@(Cart _ 'x' _) _ = c

action (Cart p        '^' l) '|'  = Cart p        '^' $ move l (-1, 0)
action (Cart p        'v' l) '|'  = Cart p        'v' $ move l (1, 0)
action (Cart p        '<' l) '-'  = Cart p        '<' $ move l ( 0,-1)
action (Cart p        '>' l) '-'  = Cart p        '>' $ move l ( 0, 1)

action (Cart p        '^' l) '/'  = Cart p        '>' $ move l ( 0, 1)
action (Cart p        '<' l) '/'  = Cart p        'v' $ move l ( 1, 0)
action (Cart p        'v' l) '/'  = Cart p        '<' $ move l ( 0,-1)
action (Cart p        '>' l) '/'  = Cart p        '^' $ move l (-1, 0)

action (Cart p        '^' l) '\\' = Cart p        '<' $ move l ( 0,-1)
action (Cart p        '>' l) '\\' = Cart p        'v' $ move l ( 1, 0)
action (Cart p        '<' l) '\\' = Cart p        '^' $ move l (-1, 0)
action (Cart p        'v' l) '\\' = Cart p        '>' $ move l ( 0, 1)

action (Cart Left     '^' l) '+'  = Cart Straight '^' $ move l (-1, 0)
action (Cart Straight '^' l) '+'  = Cart Right    '>' $ move l ( 0, 1)
action (Cart Right    '^' l) '+'  = Cart Left     '<' $ move l ( 0,-1)
action (Cart Left     'v' l) '+'  = Cart Straight 'v' $ move l ( 1, 0)
action (Cart Straight 'v' l) '+'  = Cart Right    '<' $ move l ( 0,-1)
action (Cart Right    'v' l) '+'  = Cart Left     '>' $ move l ( 0, 1)
action (Cart Left     '<' l) '+'  = Cart Straight '<' $ move l ( 0,-1)
action (Cart Straight '<' l) '+'  = Cart Right    '^' $ move l (-1, 0)
action (Cart Right    '<' l) '+'  = Cart Left     'v' $ move l ( 1, 0)
action (Cart Left     '>' l) '+'  = Cart Straight '>' $ move l ( 0, 1)
action (Cart Straight '>' l) '+'  = Cart Right    'v' $ move l ( 1, 0)
action (Cart Right    '>' l) '+'  = Cart Left     '^' $ move l (-1, 0)
-}

action = if' <$$>>> (== 'x') . _direction ... const <*< const $
         if' <$$>>> match '^' '|' <*< justMove (-1, 0) $
         if' <$$>>> match 'v' '|' <*< justMove ( 1, 0) $
         if' <$$>>> match '<' '-' <*< justMove ( 0,-1) $
         if' <$$>>> match '>' '-' <*< justMove ( 0, 1) $

         if' <$$>>> match '^' '/' <*< moveTurn ( 0, 1) '>' $
         if' <$$>>> match '<' '/' <*< moveTurn ( 1, 0) 'v' $
         if' <$$>>> match 'v' '/' <*< moveTurn ( 0,-1) '<' $
         if' <$$>>> match '>' '/' <*< moveTurn (-1, 0) '^' $

         if' <$$>>> match '^' '\\' <*< moveTurn ( 0,-1) '<' $
         if' <$$>>> match '>' '\\' <*< moveTurn ( 1, 0) 'v' $
         if' <$$>>> match '<' '\\' <*< moveTurn (-1, 0) '^' $
         if' <$$>>> match 'v' '\\' <*< moveTurn ( 0, 1) '>' $

         if' <$$>>> match2 Left     '^' '+' <*< moveTurnStep (-1, 0) '^' Straight $
         if' <$$>>> match2 Straight '^' '+' <*< moveTurnStep ( 0, 1) '>' Right    $
         if' <$$>>> match2 Right    '^' '+' <*< moveTurnStep ( 0,-1) '<' Left     $
         if' <$$>>> match2 Left     'v' '+' <*< moveTurnStep ( 1, 0) 'v' Straight $
         if' <$$>>> match2 Straight 'v' '+' <*< moveTurnStep ( 0,-1) '<' Right    $
         if' <$$>>> match2 Right    'v' '+' <*< moveTurnStep ( 0, 1) '>' Left     $
         if' <$$>>> match2 Left     '<' '+' <*< moveTurnStep ( 0,-1) '<' Straight $
         if' <$$>>> match2 Straight '<' '+' <*< moveTurnStep (-1, 0) '^' Right    $
         if' <$$>>> match2 Right    '<' '+' <*< moveTurnStep ( 1, 0) 'v' Left     $
         if' <$$>>> match2 Left     '>' '+' <*< moveTurnStep ( 0, 1) '>' Straight $
         if' <$$>>> match2 Straight '>' '+' <*< moveTurnStep ( 1, 0) 'v' Right    $
         if' <$$>>> match2 Right    '>' '+' <*< moveTurnStep (-1, 0) '^' Left     $

         error "shouldn't be here"

match :: Char -> Char -> Cart -> Char -> Bool
match = (&&) <$$$$>> ((==) <$$$$>> arg41 <*< _direction ... arg43)
                 <*< ((==) <$$$$>> arg42 <*< arg44)

match2 :: Move -> Char -> Char -> Cart -> Char -> Bool
match2 = allOf <$$$$$>>> ((==) <$$$$$>> arg52 <*< _direction ... arg54)
                     <*< ((==) <$$$$$>> arg53 <*< arg55)
                     <*< ((==) <$$$$$>> arg51 <*< _prevTurn ... arg54)

justMove :: Coordinate -> Cart -> ignore -> Cart
justMove = over loc . move &> const ... ($) <& id

moveTurn :: Coordinate -> Char -> Cart -> ignore -> Cart
moveTurn = const ... ($) . (.) <&>>> over loc . move
                                 <*< set direction
                                 <*< id

moveTurnStep :: Coordinate -> Char -> Move -> Cart -> ignore -> Cart
moveTurnStep = const ... compose3 <&>>>> over loc . move
                                     <*< set direction
                                     <*< set prevTurn
                                     <*< id

act :: Map -> Cart -> Cart
-- act map = action <$> id <*> ((map !) . loc)
act = action <$$>> arg2 <*< (id &> (!) <& _loc)

collided :: Cart -> Bool
collided = (== 'x') . _direction

findColliding :: Carts -> [Cart]
findColliding = concat . filter ((> 1) . length) . groupOn _loc . filter (not . collided) . V.toList

collidingWithIndex :: Carts -> [(Int,Cart)]
collidingWithIndex = fmap . (&&& set direction 'x') <$> fromJust ... flip V.elemIndex <*> findColliding

markColliding :: Carts -> Carts
markColliding = (V.//) <$> id <*> collidingWithIndex

step :: (Cart -> Cart) -> Carts -> Int -> Carts
--step act carts cartIndex = markColliding $ carts V.// [(cartIndex, act (carts V.! cartIndex))]
--step = markColliding ... flip (V.//) . singleton ... (,) <$$$>>> arg33 <*< (. (V.!)) . (.) <*< arg32 -- uugh...
step = markColliding ... (V.//) <$$$>> arg32 <*< ( singleton ... (,) <$$$>> arg33 <*< (... (V.!)) )

tick :: Map -> Carts -> [Carts]
--tick = take <$$>> length ... arg2
--              <*< (\map -> tail . fmap fst . iterate' (uncurry (step $ act map) &&& succ . snd) . (,0))
tick = take <$$>> length ... arg2
              <*< ((,) <$$>> uncurry . step . act <*< succ . snd ... arg2) &> tail . fmap fst ... iterate' <& (,0)

sortByLocation :: Carts -> Carts
sortByLocation = V.fromList . sortOn _loc . V.toList

ticks :: Map -> Carts -> [[Carts]]
--ticks map = iterate' (tick map . sortByLocation . last) . singleton
--ticks = (. singleton) . iterate' . (. sortByLocation . last) . tick
--ticks = iterate' <&>> (. sortByLocation . last) . tick <*< singleton
ticks = (. sortByLocation . last) . tick &> iterate' <& singleton

withCarts :: Carts -> Map -> Map
--withCarts carts = M.imap $ \i c -> case V.find ((== i) . _loc) carts of
--    Just (Cart _ d _) -> d
--    Nothing           -> c
withCarts = M.imap . (if' <$$$>>> isJust ...                (V.find <$$$>> (. _loc) . (==) ... arg32 <*< arg31)
                              <*< _direction . fromJust ... (V.find <$$$>> (. _loc) . (==) ... arg32 <*< arg31)
                              <*< arg33)

withoutCarts :: Map -> Map
--withoutCarts = M.map $ \case
--    '>' -> '-'
--    '<' -> '-'
--    '^' -> '|'
--    'v' -> '|'
--    x   -> x
withoutCarts = M.map $ if' <$> (== '>') <*> const '-' <*> (
                       if' <$> (== '<') <*> const '-' <*> (
                       if' <$> (== '^') <*> const '|' <*> (
                       if' <$> (== 'v') <*> const '|' <*>
                       id )))

solve :: [String] -> ([[Carts]], Map)
solve = ( (,) ... ticks <$> withoutCarts <*> carts <*> withoutCarts) . M.fromLists

solve1 :: [String] -> (Int, Int)
solve1 = swap . _loc . fromJust . V.find collided . head . dropWhile (null . V.filter collided) . concat . fst . solve

-- the location of the first crash
solution1 :: IO (Int, Int)
solution1 = solve1 <$> input
-- 115,138

hasMultipleCarts :: Carts -> Bool
hasMultipleCarts = (> 1) . length . V.filter (not . collided)

solve2 :: [String] -> (Int, Int)
solve2 = swap . _loc . fromJust . V.find (not . collided) . last . head . dropWhile (hasMultipleCarts . last) . fst . solve

-- What is the location of the last cart 
solution2 :: IO (Int, Int)
solution2 = solve2 <$> input
-- 0,98