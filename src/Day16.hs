{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections         #-}
module Day16 where

import           Control.Applicative (liftA2)
import           Control.Applicative.Combinators (count, between, sepBy)
import           Control.Arrow                        ((&&&))
import           Control.Conditional (if')
import           Data.Bifunctor                       (bimap)
import           Data.Bits                            ((.&.), (.|.))
import           Data.Function                        (on)
import           Data.List                            (iterate', nub, nubBy,
                                                       sortOn)
import           Data.List.Split                      (chunksOf)
import           Data.Maybe                           (catMaybes, fromJust,
                                                       fromMaybe, isJust,
                                                       isNothing, listToMaybe,
                                                       mapMaybe, maybeToList)
import           Data.Ord                             (comparing)
import qualified Data.Sequence                        as S
import           Data.Sequence                        (Seq)
import           Data.Tuple.Extra (uncurry3)
import           Numeric.Natural
import           Text.Megaparsec            (Parsec, anySingle, many, optional,
                                             parseMaybe, try, (<|>))
import           Text.Megaparsec.Char       (char, letterChar, space, string)
import           Text.Megaparsec.Char.Lexer (decimal, signed)
import           Universum.VarArg ((...))
import           Util
import Data.Composition ((.**), (.*))

input :: IO [String]
input = lines <$> readFile  "input/input16.txt"

input2 :: IO [String]
input2 = lines <$> readFile  "input/input16_2.txt"

type Parser = Parsec () String

regsP :: String -> Parser [Integer]
regsP = (*> between (char '[') (char ']') (decimal `sepBy` string ", ")) . string

beforeP :: Parser Registers
beforeP = S.fromList . fmap Register <$> regsP "Before: "

afterP :: Parser Registers
afterP = S.fromList . fmap Register <$> regsP "After:  "

instrP :: Parser (Int, Integer, Integer, Integer)
instrP = (,,,) <$> (decimal <* char ' ') <*> (decimal <* char ' ') <*> (decimal <* char ' ') <*> decimal

before :: String -> Registers
before = fromJust ... parseMaybe beforeP

after :: String -> Registers
after = fromJust ... parseMaybe afterP

instr :: String -> (Int, Integer, Integer, Integer)
instr = fromJust ... parseMaybe instrP

parseData :: [String] -> [((Int, Integer, Integer, Integer), Registers, Registers)]
parseData = fmap ((,,) <$> instr . head . tail
                       <*> before . head
                       <*> after . head . tail . tail) . chunksOf 4

newtype Register = Register { value :: Integer}
    deriving Eq

instance Show Register where
    show (Register v) = show v

type Registers = Seq Register

data Input = Reg {getReg :: Natural} | Val { getVal :: Integer}

data Opcode = AddR | AddI |
              MulR | MulI |
              BanR | BanI |
              BorR | BorI |
              SetR | SetI |
              GtIR | GtRI | GtRR |
              EqIR | EqRI | EqRR
    deriving (Show,Eq,Ord)

opcodes = [AddR,AddI,MulR,MulI,BanR,BanI,BorR,BorI,SetR,SetI,GtIR,GtRI,GtRR,EqIR,EqRI,EqRR]

data Instruction = Instruction {
    opcode :: Opcode,
    a :: Input,
    b :: Input,
    c :: Input
}

(!) :: Registers -> Natural -> Integer
(!) = value ... S.index <&>> id <*< fromIntegral

update' :: Input -> Integer -> Registers -> Registers
update' = S.update <&>> fromIntegral . getReg <*< Register

-- not gonna make this point free...
behave :: Instruction -> Registers -> Registers
behave (Instruction AddR (Reg a) (Reg b) c) regs = update' c (regs ! a + regs !   b) regs
behave (Instruction AddI (Reg a) (Val b) c) regs = update' c (regs ! a +          b) regs
behave (Instruction MulR (Reg a) (Reg b) c) regs = update' c (regs ! a * regs !   b) regs
behave (Instruction MulI (Reg a) (Val b) c) regs = update' c (regs ! a *          b) regs
behave (Instruction BanR (Reg a) (Reg b) c) regs = update' c (regs ! a .&. regs ! b) regs
behave (Instruction BanI (Reg a) (Val b) c) regs = update' c (regs ! a .&.        b) regs
behave (Instruction BorR (Reg a) (Reg b) c) regs = update' c (regs ! a .|. regs ! b) regs
behave (Instruction BorI (Reg a) (Val b) c) regs = update' c (regs ! a .|.        b) regs
behave (Instruction SetR (Reg a) _       c) regs = update' c (regs ! a             ) regs
behave (Instruction SetI (Val a) _       c) regs = update' c         a               regs
behave (Instruction GtIR (Val a) (Reg b) c) regs = update' c (if a        >  regs ! b then 1 else 0) regs
behave (Instruction GtRI (Reg a) (Val b) c) regs = update' c (if regs ! a >  b        then 1 else 0) regs
behave (Instruction GtRR (Reg a) (Reg b) c) regs = update' c (if regs ! a >  regs ! b then 1 else 0) regs
behave (Instruction EqIR (Val a) (Reg b) c) regs = update' c (if a        == regs ! b then 1 else 0) regs
behave (Instruction EqRI (Reg a) (Val b) c) regs = update' c (if regs ! a == b        then 1 else 0) regs
behave (Instruction EqRR (Reg a) (Reg b) c) regs = update' c (if regs ! a == regs ! b then 1 else 0) regs

reg :: Integer -> Input
reg = Reg . fromIntegral

-- not gonna make this point free...
mkInstruction :: Opcode -> Integer -> Integer -> Integer -> Instruction
mkInstruction AddR a b c = Instruction AddR (reg a) (reg b) (reg c)
mkInstruction AddI a b c = Instruction AddI (reg a) (Val b) (reg c)
mkInstruction MulR a b c = Instruction MulR (reg a) (reg b) (reg c)
mkInstruction MulI a b c = Instruction MulI (reg a) (Val b) (reg c)
mkInstruction BanR a b c = Instruction BanR (reg a) (reg b) (reg c)
mkInstruction BanI a b c = Instruction BanI (reg a) (Val b) (reg c)
mkInstruction BorR a b c = Instruction BorR (reg a) (reg b) (reg c)
mkInstruction BorI a b c = Instruction BorI (reg a) (Val b) (reg c)
mkInstruction SetR a b c = Instruction SetR (reg a) (Val b) (reg c)
mkInstruction SetI a b c = Instruction SetI (Val a) (Val b) (reg c)
mkInstruction GtIR a b c = Instruction GtIR (Val a) (reg b) (reg c)
mkInstruction GtRI a b c = Instruction GtRI (reg a) (Val b) (reg c)
mkInstruction GtRR a b c = Instruction GtRR (reg a) (reg b) (reg c)
mkInstruction EqIR a b c = Instruction EqIR (Val a) (reg b) (reg c)
mkInstruction EqRI a b c = Instruction EqRI (reg a) (Val b) (reg c)
mkInstruction EqRR a b c = Instruction EqRR (reg a) (reg b) (reg c)

matches :: (a, Integer, Integer, Integer) -> Registers -> Registers -> Opcode -> Bool
matches = (==) <$$$$>> arg43
                   <*< (behave <$$$$>> (mkInstruction <$$$$>>>> arg44 <*< snd4 ... arg41 <*< thd4 ... arg41 <*< fth4 ... arg41)
                                   <*< arg42)

solve1 = length . filter (>= 3) . fmap (length . filter id . flip fmap opcodes . uncurry3 matches) . parseData

-- how many samples in your puzzle input behave like three or more opcodes
solution1 = solve1 <$> input
-- 542

baz :: (t, Integer, Integer, Integer) -> Registers -> Registers -> [Maybe (t, Opcode)]
baz = flip fmap opcodes .** (if' <$$$$>>> matches
                                         <*< (Just ... (,) <$$$$>> fst4 ... arg41 <*< arg44)
                                         <*< const4 Nothing) -- uuh...

opcodeCandidates :: [String] -> [[(Int, Opcode)]]
opcodeCandidates = nub . sortOn length . fmap (catMaybes . uncurry3 baz) . parseData

quux :: (Eq a, Eq b) => a -> b -> [(a, b)] -> [(a, b)]
quux = filter .* (liftA2 (&&) <&>> (. fst) . (/=)
                              <*< (. snd) . (/=))

foo :: (Eq a, Eq b) => [[(a, b)]] -> [[(a, b)]]
foo = sortOn length . filter (not . null) ... fmap . uncurry quux <$> head . head <*> tail

bar :: (Eq a, Eq b) => [[(a, b)]] -> [(a, b)] -> ([[(a, b)]], [(a, b)])
bar = if' <$$>>> null ... const
             <*< (,)
             <*< ( (,) <$$>> foo .* const
                         <*< ( (:) <&>> head . head <*< id) )

matchOpcodes :: [String] -> [(Int, Opcode)]
matchOpcodes = snd . head . filter (null . fst) . iterate' (uncurry bar) . (,[]) . opcodeCandidates

qux :: [(Int, Opcode)] -> [(Int, Integer, Integer, Integer)] -> [Instruction]
qux = fmap . uncurry4 . flip (mkInstruction . snd . head ... filter . (. fst) . (==))

solve2 :: ([String], [String]) -> Integer
solve2 = flip S.index 0 . fmap value . foldl (flip behave) (S.fromList $ fmap Register [0,0,0,0]) . uncurry qux . bimap matchOpcodes (fmap instr)

solution2 = solve2 <$> ((,) <$> input <*> input2)
-- 575