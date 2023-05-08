module Day08 where

import           Control.Applicative (liftA2)
import           Control.Applicative.Combinators (count, optional)
import           Control.Conditional (if')
import           Data.Function (on)
import           Data.Maybe (fromJust)
import           Text.Megaparsec (Parsec, parseMaybe)
import           Text.Megaparsec.Char (char)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Universum ((...))
import           Util ((<$$>>), (<&>>), (<*<), arg2, (<&), (&>))


input :: IO String
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
headerP = Header <$> (optional (char ' ') *> decimal <* char ' ')
                 <*> decimal

metadataP :: Parser Int
metadataP = char ' ' *> decimal

nodeP :: Parser Node
--nodeP = do
--    Header c m <- headerP
--    children <- count c nodeP
--    metadata <- count m metadataP
--    pure $ Node children metadata
nodeP = headerP >>= (liftA2 Node <$> (`count` nodeP) . childNodes
                                 <*> (`count` metadataP) . metadataNodes)

tree :: String -> Node
tree = fromJust . parseMaybe nodeP

sumMetadata :: Node -> Int
sumMetadata = ((+) `on` sum) <$> fmap sumMetadata . children <*> metadata

solve1 :: String -> Int
solve1 = sumMetadata . tree

value :: Node -> Int
--value (Node [] metadata)       = sum metadata
--value (Node children metadata) = (sum . fmap (value . (\m -> children !! (m-1))) . filter (\m -> m > 0 && m <= length children)) metadata
value = sum ... if' <$> null . children
                    <*> metadata
                    <*> ( ((.) <$> mapToValueOfReferredChild <*> metadataEntriesReferringToChild) <$> children <*> metadata )

mapToValueOfReferredChild :: [Node] -> [Int] -> [Int]
mapToValueOfReferredChild = fmap . (value ... (id &> (!!) <& pred))

metadataEntriesReferringToChild :: [a] -> [Int] -> [Int]
metadataEntriesReferringToChild = filter . ((&&) <$$>> (> 0) ... arg2 <*< (>=) . length)

solve2 :: String -> Int
solve2 = value . tree

-- What is the sum of all metadata entries?
solution1 :: IO Int
solution1 = solve1 <$> input
-- 46781

-- What is the value of the root node?
solution2 :: IO Int
solution2 = solve2 <$> input
-- 21405