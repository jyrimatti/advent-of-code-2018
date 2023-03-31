{-# LANGUAGE TupleSections #-}
module Day05 where

import Control.Arrow       ((&&&))
import Control.Conditional (if')
import Data.Char           (isUpper, toLower, toUpper)
import Data.List           (find, iterate', nub)
import Data.Maybe          (fromJust, fromMaybe)
import Universum.VarArg               ( (...) )
import Util ( (<$$>>), (<$$>>>), (<&>>), (<*<), arg2, singleton )

input :: IO String
input = readFile "input/input05.txt"

toggleCase :: Char -> Char
toggleCase = if' <$> isUpper <*> toLower <*> toUpper

data Reaction = Reacted | Stable
    deriving Eq

--react :: Char -> (Reaction, String) -> (Reaction, String)
--react a (b,[])                           = (b      , [a]    )
--react a (b,bs) | toggleCase a == head bs = (Reacted, tail bs)
--react a (b,bs)                           = (b      , a : bs )

react :: Char -> (Reaction, String) -> (Reaction, String)
react = if' <$$>>> (== "") . snd ... arg2
               <*< ((,) <$$>> fst ... arg2 <*< singleton ... const) $
        if' <$$>>> ((==) <&>> toggleCase <*< head . snd)
               <*< (Reacted,) . tail . snd ... arg2 $
                   (,) <$$>> fst ... arg2 <*< ((:) <&>> id <*< snd)

act :: (a, String) -> (Reaction, String)
act = foldr react (Stable,[]) . snd

solve1 :: String -> Int
solve1 = length . snd . fromJust . find ((== Stable) . fst) . iterate' act . (Reacted,)

-- How many units remain after fully reacting the polymer you scanned?
solution1 :: IO Int
solution1 = solve1 <$> input
-- 10762


removeProblematic :: String -> String -> [String]
--removeProblematic inp = fmap (\problematic -> filter ((/= problematic) . toLower) inp)
removeProblematic = fmap . ( flip filter <&>> id <*< (. toLower) . (/=) )

solve2 :: String -> Int
solve2 = minimum . fmap solve1 . (removeProblematic <$> id <*> (nub . fmap toLower))

-- What is the length of the shortest polymer you can produce
solution2 :: IO Int
solution2 = solve2 <$> input
-- 6946
