{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE InstanceSigs #-}
module Day16 where

import           Control.Applicative (liftA2)
import           Control.Applicative.Combinators (between, sepBy)
import           Control.Conditional (if', bool)
import           Data.Composition ((.**), (.*))
import           Data.Bifunctor (bimap)
import           Data.Bits ((.&.), (.|.))
import           Data.List (iterate', sortOn)
import           Data.List.Extra (nubOrd)
import           Data.List.Split (chunksOf)
import           Data.Maybe (catMaybes, fromJust)
import qualified Data.Sequence as S
import           Data.Sequence (Seq)
import           Data.Tuple.Extra (uncurry3)
import           Numeric.Natural (Natural)
import           Text.Megaparsec (Parsec, parseMaybe)
import           Text.Megaparsec.Char (char, string)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Universum ((...))
import           Util ((<$$$$>>), (<$$$$>>>), (<$$$$>>>>), (<$$>>), (<$$>>>)
                     , (<&>>), (<*<), arg41, arg42, arg43, arg44, const4, fst4
                     , fth4, snd4, thd4, uncurry4, (<&), (&>), (<&>>>>), arg1
                     , arg2, (<$$$$$>>>), arg51, arg55, arg53, arg52, arg54
                     , (<$$$$$>>), const2, const5, (<$$>>>>>))


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
    show :: Register -> String
    show = show . value

type Registers = Seq Register

data Input = Reg { getReg :: Natural } | Val { getVal :: Integer}

data Opcode = AddR | AddI |
              MulR | MulI |
              BanR | BanI |
              BorR | BorI |
              SetR | SetI |
              GtIR | GtRI | GtRR |
              EqIR | EqRI | EqRR
    deriving (Show,Eq,Ord)

opcodes :: [Opcode]
opcodes = [AddR,AddI,MulR,MulI,BanR,BanI,BorR,BorI,SetR,SetI,GtIR,GtRI,GtRR,EqIR,EqRI,EqRR]

data Instruction = Instruction {
    opcode :: Opcode,
    a :: Input,
    b :: Input,
    c :: Input
}

(!) :: Registers -> Natural -> Integer
(!) = id &> value ... S.index <& fromIntegral

update' :: Input -> Integer -> Registers -> Registers
update' = fromIntegral . getReg &> S.update <& Register

-- some renaming for 'behave'
_opcode :: p1 -> p2 -> p3 -> p4 -> p5 -> p1
_opcode = arg51
_a :: p1 -> p2 -> p3 -> p4 -> p5 -> p2
_a = arg52
_b :: p1 -> p2 -> p3 -> p4 -> p5 -> p3
_b = arg53
_c :: p1 -> p2 -> p3 -> p4 -> p5 -> p4
_c = arg54
_regs :: p1 -> p2 -> p3 -> p4 -> p5 -> p5
_regs = arg55

_getReg :: (Opcode -> Input -> Input -> Input -> Registers -> Input) -> (Opcode -> Input -> Input -> Input -> Registers -> Integer)
_getReg = ((!) <$$$$$>> _regs <*<) . (getReg ...)

behave :: Instruction -> Registers -> Registers
behave = behav_ <$$>>>>> opcode ... arg1 <*< a ... arg1 <*< b ... arg1 <*< c ... arg1 <*< arg2
--behave (Instruction AddR (Reg a) (Reg b) c) regs = update' c (regs ! a + regs !   b) regs
--behave (Instruction AddI (Reg a) (Val b) c) regs = update' c (regs ! a +          b) regs
--behave (Instruction MulR (Reg a) (Reg b) c) regs = update' c (regs ! a * regs !   b) regs
--behave (Instruction MulI (Reg a) (Val b) c) regs = update' c (regs ! a *          b) regs
--behave (Instruction BanR (Reg a) (Reg b) c) regs = update' c (regs ! a .&. regs ! b) regs
--behave (Instruction BanI (Reg a) (Val b) c) regs = update' c (regs ! a .&.        b) regs
--behave (Instruction BorR (Reg a) (Reg b) c) regs = update' c (regs ! a .|. regs ! b) regs
--behave (Instruction BorI (Reg a) (Val b) c) regs = update' c (regs ! a .|.        b) regs
--behave (Instruction SetR (Reg a) _       c) regs = update' c (regs ! a             ) regs
--behave (Instruction SetI (Val a) _       c) regs = update' c         a               regs
--behave (Instruction GtIR (Val a) (Reg b) c) regs = update' c (if a        >  regs ! b then 1 else 0) regs
--behave (Instruction GtRI (Reg a) (Val b) c) regs = update' c (if regs ! a >  b        then 1 else 0) regs
--behave (Instruction GtRR (Reg a) (Reg b) c) regs = update' c (if regs ! a >  regs ! b then 1 else 0) regs
--behave (Instruction EqIR (Val a) (Reg b) c) regs = update' c (if a        == regs ! b then 1 else 0) regs
--behave (Instruction EqRI (Reg a) (Val b) c) regs = update' c (if regs ! a == b        then 1 else 0) regs
--behave (Instruction EqRR (Reg a) (Reg b) c) regs = update' c (if regs ! a == regs ! b then 1 else 0) regs

behav_ :: Opcode -> Input -> Input -> Input -> Registers -> Registers
behav_ = if' <$$$$$>>> (== AddR) ... _opcode
                   <*< (update' <$$$$$>>> _c <*<             ( (+)  <$$$$$>> _getReg    _a <*< _getReg    _b) <*< _regs) $
         if' <$$$$$>>> (== AddI) ... _opcode
                   <*< (update' <$$$$$>>> _c <*<             ( (+)  <$$$$$>> _getReg    _a <*< getVal ... _b) <*< _regs) $
         if' <$$$$$>>> (== MulR) ... _opcode
                   <*< (update' <$$$$$>>> _c <*<             ( (*)  <$$$$$>> _getReg    _a <*< _getReg    _b) <*< _regs) $
         if' <$$$$$>>> (== MulI) ... _opcode
                   <*< (update' <$$$$$>>> _c <*<             ( (*)  <$$$$$>> _getReg    _a <*< getVal ... _b) <*< _regs) $
         if' <$$$$$>>> (== BanR) ... _opcode
                   <*< (update' <$$$$$>>> _c <*<             ((.&.) <$$$$$>> _getReg    _a <*< _getReg    _b) <*< _regs) $
         if' <$$$$$>>> (== BanI) ... _opcode
                   <*< (update' <$$$$$>>> _c <*<             ((.&.) <$$$$$>> _getReg    _a <*< getVal ... _b) <*< _regs) $
         if' <$$$$$>>> (== BorR) ... _opcode
                   <*< (update' <$$$$$>>> _c <*<             ((.|.) <$$$$$>> _getReg    _a <*< _getReg    _b) <*< _regs) $
         if' <$$$$$>>> (== BorI) ... _opcode
                   <*< (update' <$$$$$>>> _c <*<             ((.|.) <$$$$$>> _getReg    _a <*< getVal ... _b) <*< _regs) $
         if' <$$$$$>>> (== SetR) ... _opcode
                   <*< (update' <$$$$$>>> _c <*<                             _getReg    _a                    <*< _regs) $
         if' <$$$$$>>> (== SetI) ... _opcode
                   <*< (update' <$$$$$>>> _c <*<                             getVal ... _a                    <*< _regs) $
         if' <$$$$$>>> (== GtIR) ... _opcode
                   <*< (update' <$$$$$>>> _c <*< (bool 0 1 ... (>)  <$$$$$>> getVal ... _a <*< _getReg    _b) <*< _regs) $
         if' <$$$$$>>> (== GtRI) ... _opcode
                   <*< (update' <$$$$$>>> _c <*< (bool 0 1 ... (>)  <$$$$$>> _getReg    _a <*< getVal ... _b) <*< _regs) $
         if' <$$$$$>>> (== GtRR) ... _opcode
                   <*< (update' <$$$$$>>> _c <*< (bool 0 1 ... (>)  <$$$$$>> _getReg    _a <*< _getReg    _b) <*< _regs) $
         if' <$$$$$>>> (== EqIR) ... _opcode
                   <*< (update' <$$$$$>>> _c <*< (bool 0 1 ... (==) <$$$$$>> getVal ... _a <*< _getReg    _b) <*< _regs) $
         if' <$$$$$>>> (== EqRI) ... _opcode
                   <*< (update' <$$$$$>>> _c <*< (bool 0 1 ... (==) <$$$$$>> _getReg    _a <*< getVal ... _b) <*< _regs) $
         if' <$$$$$>>> (== EqRR) ... _opcode
                   <*< (update' <$$$$$>>> _c <*< (bool 0 1 ... (==) <$$$$$>> _getReg    _a <*< _getReg    _b) <*< _regs)
                   <*< error "not here"

reg :: Integer -> Input
reg = Reg . fromIntegral

mkInstruction :: Opcode -> Integer -> Integer -> Integer -> Instruction
--mkInstruction AddR a b c = Instruction AddR (reg a) (reg b) (reg c)
--mkInstruction AddI a b c = Instruction AddI (reg a) (Val b) (reg c)
--mkInstruction MulR a b c = Instruction MulR (reg a) (reg b) (reg c)
--mkInstruction MulI a b c = Instruction MulI (reg a) (Val b) (reg c)
--mkInstruction BanR a b c = Instruction BanR (reg a) (reg b) (reg c)
--mkInstruction BanI a b c = Instruction BanI (reg a) (Val b) (reg c)
--mkInstruction BorR a b c = Instruction BorR (reg a) (reg b) (reg c)
--mkInstruction BorI a b c = Instruction BorI (reg a) (Val b) (reg c)
--mkInstruction SetR a b c = Instruction SetR (reg a) (Val b) (reg c)
--mkInstruction SetI a b c = Instruction SetI (Val a) (Val b) (reg c)
--mkInstruction GtIR a b c = Instruction GtIR (Val a) (reg b) (reg c)
--mkInstruction GtRI a b c = Instruction GtRI (reg a) (Val b) (reg c)
--mkInstruction GtRR a b c = Instruction GtRR (reg a) (reg b) (reg c)
--mkInstruction EqIR a b c = Instruction EqIR (Val a) (reg b) (reg c)
--mkInstruction EqRI a b c = Instruction EqRI (reg a) (Val b) (reg c)
--mkInstruction EqRR a b c = Instruction EqRR (reg a) (reg b) (reg c)
mkInstruction = if' <$$$$>>> (== AddR) ... arg41 <*< (Instruction <&>>>> id <*< reg <*< reg <*< reg) $
                if' <$$$$>>> (== AddI) ... arg41 <*< (Instruction <&>>>> id <*< reg <*< Val <*< reg) $
                if' <$$$$>>> (== MulR) ... arg41 <*< (Instruction <&>>>> id <*< reg <*< reg <*< reg) $
                if' <$$$$>>> (== MulI) ... arg41 <*< (Instruction <&>>>> id <*< reg <*< Val <*< reg) $
                if' <$$$$>>> (== BanR) ... arg41 <*< (Instruction <&>>>> id <*< reg <*< reg <*< reg) $
                if' <$$$$>>> (== BanI) ... arg41 <*< (Instruction <&>>>> id <*< reg <*< Val <*< reg) $
                if' <$$$$>>> (== BorR) ... arg41 <*< (Instruction <&>>>> id <*< reg <*< reg <*< reg) $
                if' <$$$$>>> (== BorI) ... arg41 <*< (Instruction <&>>>> id <*< reg <*< Val <*< reg) $
                if' <$$$$>>> (== SetR) ... arg41 <*< (Instruction <&>>>> id <*< reg <*< Val <*< reg) $
                if' <$$$$>>> (== SetI) ... arg41 <*< (Instruction <&>>>> id <*< Val <*< Val <*< reg) $
                if' <$$$$>>> (== GtIR) ... arg41 <*< (Instruction <&>>>> id <*< Val <*< reg <*< reg) $
                if' <$$$$>>> (== GtRI) ... arg41 <*< (Instruction <&>>>> id <*< reg <*< Val <*< reg) $
                if' <$$$$>>> (== GtRR) ... arg41 <*< (Instruction <&>>>> id <*< reg <*< reg <*< reg) $
                if' <$$$$>>> (== EqIR) ... arg41 <*< (Instruction <&>>>> id <*< Val <*< reg <*< reg) $
                if' <$$$$>>> (== EqRI) ... arg41 <*< (Instruction <&>>>> id <*< reg <*< Val <*< reg) $
                if' <$$$$>>> (== EqRR) ... arg41 <*< (Instruction <&>>>> id <*< reg <*< reg <*< reg)
                         <*< error "not here"

matches :: (a, Integer, Integer, Integer) -> Registers -> Registers -> Opcode -> Bool
matches = (==) <$$$$>> arg43
                   <*< (behave <$$$$>> (mkInstruction <$$$$>>>> arg44 <*< snd4 ... arg41 <*< thd4 ... arg41 <*< fth4 ... arg41)
                                   <*< arg42)

solve1 :: [String] -> Int
solve1 = length . filter (>= 3) . fmap (length . filter id . (<$> opcodes) . uncurry3 matches) . parseData

-- how many samples in your puzzle input behave like three or more opcodes
solution1 :: IO Int
solution1 = solve1 <$> input
-- 542

baz :: (t, Integer, Integer, Integer) -> Registers -> Registers -> [Maybe (t, Opcode)]
baz = (<$> opcodes) .** (if' <$$$$>>> matches
                                  <*< (Just ... (,) <$$$$>> fst4 ... arg41 <*< arg44)
                                  <*< const4 Nothing)

opcodeCandidates :: [String] -> [[(Int, Opcode)]]
opcodeCandidates = nubOrd . sortOn length . fmap (catMaybes . uncurry3 baz) . parseData

quux :: (Eq a, Eq b) => a -> b -> [(a, b)] -> [(a, b)]
quux = (. fst) . (/=) &> filter .* liftA2 (&&) <& (. snd) . (/=)

foo :: (Eq a, Eq b) => [[(a, b)]] -> [[(a, b)]]
foo = sortOn length . filter (not . null) ... fmap . uncurry quux <$> head . head <*> tail

bar :: (Eq a, Eq b) => [[(a, b)]] -> [(a, b)] -> ([[(a, b)]], [(a, b)])
bar = if' <$$>>> null ... const
             <*< (,)
             <*< ( (,) <$$>> foo .* const
                         <*< head . head &> (:) <& id)

matchOpcodes :: [String] -> [(Int, Opcode)]
matchOpcodes = snd . head . filter (null . fst) . iterate' (uncurry bar) . (,[]) . opcodeCandidates

qux :: [(Int, Opcode)] -> [(Int, Integer, Integer, Integer)] -> [Instruction]
qux = fmap . uncurry4 . flip (mkInstruction . snd . head ... filter . (. fst) . (==))

solve2 :: ([String], [String]) -> Integer
solve2 = (`S.index` 0) . fmap value . foldl (flip behave) (S.fromList $ fmap Register [0,0,0,0]) . uncurry qux . bimap matchOpcodes (fmap instr)

solution2 :: IO Integer
solution2 = solve2 <$> ((,) <$> input <*> input2)
-- 575