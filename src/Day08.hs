module Day08 where

import Control.Applicative (liftA2)
import Control.Applicative.Combinators (count)
import Control.Category ((<<<))
import Control.Conditional (if')
import Control.Monad ((<=<))
import Data.Maybe (fromJust)
import Text.Megaparsec            (Parsec, anySingle, many, optional,
                                             parseMaybe, try, (<|>))
import Text.Megaparsec.Char       (char, letterChar, space, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Universum.VarArg ((...))
import Util

input = readFile "input/input08.txt"

data Header = Header {
    childNodes :: Int,
    metadataNodes :: Int
}

data Node = Node {
    children :: [Node],
    metadata :: [Int]
} deriving Show

type Parser = Parsec () String

headerP :: Parser Header
headerP = Header <$> (optional (char ' ') *> decimal <* char ' ') <*> decimal

metadataP :: Parser Int
metadataP = char ' ' *> decimal

nodeP :: Parser Node
--nodeP = do
--    Header c m <- headerP
--    children <- count c nodeP
--    metadata <- count m metadataP
--    pure $ Node children metadata
nodeP = headerP >>= (liftA2 Node <$> ($ nodeP) . count . childNodes <*> ($ metadataP) . count . metadataNodes)

tree :: String -> Node
tree = fromJust . parseMaybe nodeP

sumMetadata :: Node -> Int
sumMetadata = (+) `oN` sum <$> fmap sumMetadata . children <*> metadata

solve1 = sumMetadata . tree

value :: Node -> Int
value (Node [] metadata)       = sum metadata
value (Node children metadata) = (sum . fmap (value . (\m -> children !! (m-1))) . filter (\m -> m > 0 && m <= length children)) metadata

value2 :: Node -> Int
value2 = sum ... if' <$> null . children
                     <*> metadata
                     <*> ( ((.) <$> mapToValueOfReferredChild <*> metadataEntriesReferringToChild) <$> children <*> metadata )

mapToValueOfReferredChild :: [Node] -> [Int] -> [Int]
mapToValueOfReferredChild = fmap . (value ... (!!) <&>> id <*< pred)

metadataEntriesReferringToChild :: [a] -> [Int] -> [Int]
metadataEntriesReferringToChild = filter . ((&&) <$$>> (> 0) ... arg2 <*< flip (<=) . length)

solve2 = value . tree

-- What is the sum of all metadata entries?
solution1 = solve1 <$> input
-- 46781

-- What is the value of the root node?
solution2 = solve2 <$> input
-- 21405