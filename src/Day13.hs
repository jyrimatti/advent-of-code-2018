{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
module Day13 where

import           Control.Arrow                        ((&&&))
import           Data.Bifunctor                       (second)
import           Data.Function                        (on)
import           Data.List                            (cycle, groupBy, iterate',
                                                       nub, nubBy, sort, sortBy,
                                                       (\\))
import qualified Data.Matrix                          as M
import           Data.Matrix                          (Matrix, (!))
import           Data.Maybe                           (fromJust, isJust,
                                                       isNothing, listToMaybe)
import           Data.Tuple                           (swap)
import qualified Data.Vector                          as V
import           Data.Vector                          (Vector)
import           Prelude                              hiding
                                                       (Either (Left, Right))


input = lines <$> readFile "input/input13.txt"

isCart '<' = True
isCart '>' = True
isCart '^' = True
isCart 'v' = True
isCart _ = False

data Move = Left | Straight | Right deriving (Show, Eq)

data Cart = Cart {
    prevTurn :: Move,
    direction :: Char,
    loc :: (Int,Int)
} deriving (Show, Eq)

type Map = Matrix Char
type Carts = Vector Cart
type Coordinate = (Int,Int)

carts :: Map -> Carts
carts = V.fromList . concatMap (fmap (uncurry (Cart Right) . fromJust) . filter isJust) . M.toLists . M.imap (\index symbol -> if isCart symbol then Just (symbol,index) else Nothing)

move (r,c) (dr,dc) = (r + dr,c + dc)

-- keep collided as-is
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

act :: Map -> Cart -> Cart
act map = action <$> id <*> ((map !) . loc)

collided = (== 'x') . direction

findColliding :: Carts -> [Cart]
findColliding = concat . filter ((> 1) . length) . groupBy (on (==) loc) . filter (not . collided) . V.toList

markColliding carts = carts V.// fmap (\colliding -> (fromJust $ V.elemIndex colliding carts, colliding { direction = 'x' })) (findColliding carts)

step :: Map -> Carts -> Int -> Carts
step map carts cartIndex = markColliding $ carts V.// [(cartIndex, act map (carts V.! cartIndex))]

tick :: Map -> Carts -> [Carts]
tick map = take <$> length <*> tail . fmap fst . iterate' (uncurry (step map) &&& (+1) . snd) . (,0)

sortByLocation = V.fromList . sortBy (compare `on` loc) . V.toList

ticks :: Map -> Carts -> [[Carts]]
ticks map = iterate' (tick map . sortByLocation . last) . (: [])

withCarts :: Carts -> Map -> Map
withCarts carts = M.imap $ \i c -> case V.find ((== i) . loc) carts of
    Just (Cart _ d _) -> d
    Nothing           -> c

withoutCarts = M.map $ \case
    '>' -> '-'
    '<' -> '-'
    '^' -> '|'
    'v' -> '|'
    x   -> x

solve = (uncurry ticks &&& fst) . (withoutCarts &&& carts) . M.fromLists

solve1 = swap . loc . fromJust . V.find collided . head . dropWhile (null . V.filter collided) . concat . fst . solve

debug d = M.toLists . (!! d) . (\(res,map) -> (`withCarts` map) <$> concat res) . solve <$> input >>= mapM putStrLn

-- the location of the first crash
solution1 = solve1 <$> input
-- 115,138

hasMultipleCarts = (> 1) . length . V.filter (not . collided)

solve2 = swap . loc . fromJust . V.find (not . collided) . last . head . dropWhile (hasMultipleCarts . last) . fst . solve

-- What is the location of the last cart 
solution2 = solve2 <$> input
-- 0,98