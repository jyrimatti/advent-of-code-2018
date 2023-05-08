module Day01 where

import           Control.Conditional (if')
import           Data.Maybe (mapMaybe)
import           Data.Set (Set, empty, insert)
import           Universum ((...))
import           Util (const2, (<$$>>), (<$$>>>), (<*<))


type Elem = Int

input :: IO [Elem]
input = fmap parseElem . lines <$> readFile "input/input01.txt"
-- same as: fmap (fmap parseElem . lines) $ readFile "input/input01.txt"

parseElem :: String -> Elem
parseElem = read . filter (/= '+')

-- what is the resulting frequency
solution1 :: IO Elem
solution1 = sum <$> input
-- 400


findElem :: Elem -> Set Elem -> Maybe Elem
findElem = if' <$$>>> elem
                  <*< Just ... const
                  <*< const2 Nothing

markIfContains :: Elem -> Set Elem -> (Maybe Elem, Set Elem)
markIfContains = (,) <$$>> findElem <*< insert

solve2 :: [Elem] -> Elem
solve2 = head . mapMaybe fst . scanl (flip markIfContains . snd) (Nothing,empty) . scanl1 (+) . cycle

-- What is the first frequency your device reaches twice?
solution2 :: IO Elem
solution2 = solve2 <$> input
-- 232
