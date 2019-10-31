module Day08 where

import Text.Parsec                          (Parsec, count, many,
                                                       many1, optional, parse)
import Text.Parsec.Char                     (anyChar, char, digit,
                                                       letter, space, string)
import Text.Parsec.Combinator               (between, sepBy)
import Text.ParserCombinators.Parsec.Number (int)


input = readFile "input/input08.txt"

data Header = Header {
    childNodes :: Int,
    metadataNodes :: Int
}

data Node = Node {
    children :: [Node],
    metadata :: [Int]
} deriving Show

headerP = Header <$> (optional (char ' ') *> int <* char ' ') <*> int

metadataP = char ' ' *> int

nodeP = do
    Header c m <- headerP
    children <- count c nodeP
    metadata <- count m metadataP
    pure $ Node children metadata

tree = either undefined id . parse nodeP ""

sumMetadata (Node children metadata) = sum metadata + sum (sumMetadata <$> children)

solve1 = sumMetadata . tree

value (Node [] metadata)       = sum metadata
value (Node children metadata) = (sum . fmap (value . (\m -> children !! (m-1))) . filter (\m -> m > 0 && m <= length children)) metadata

solve2 = value . tree

-- What is the sum of all metadata entries?
solution1 = solve1 <$> input
-- 46781

-- What is the value of the root node?
solution2 = solve2 <$> input
-- 21405