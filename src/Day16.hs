{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections         #-}
module Day16 where

import           Control.Applicative.Combinators (count, between, sepBy)
import           Control.Arrow                        ((&&&))
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
import           Numeric.Natural
import           Text.Megaparsec            (Parsec, anySingle, many, optional,
                                             parseMaybe, try, (<|>))
import           Text.Megaparsec.Char       (char, letterChar, space, string)
import           Text.Megaparsec.Char.Lexer (decimal, signed)


input = lines <$> readFile  "input/input16.txt"
input2 = lines <$> readFile  "input/input16_2.txt"

type Parser = Parsec () String

foo :: String -> Parser [Integer]
foo txt = string txt *> between (char '[') (char ']') (decimal `sepBy` string ", ")
beforeP = S.fromList . fmap Register <$> foo "Before: "
afterP = S.fromList . fmap Register <$> foo "After:  "

instrP :: Parser (Int, Integer, Integer, Integer)
instrP = (,,,) <$> (decimal <* char ' ') <*> (decimal <* char ' ') <*> (decimal <* char ' ') <*> decimal

ps p = fromJust . parseMaybe p
before = ps beforeP
after = ps afterP
instr = ps instrP

parseData = fmap (\(a:b:c:_) -> (before a, instr b, after c)) . chunksOf 4

newtype Register = Register { value :: Integer} deriving Eq
instance Show Register where
    show (Register v) = show v

type Registers = S.Seq Register

data Input = Reg Natural | Val Integer

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
(!) regs i = value $ S.index regs (fromIntegral i)

update' :: Input -> Integer -> Registers -> Registers
update' (Reg i) val = S.update (fromIntegral i) (Register val)

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

reg = Reg . fromIntegral

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

matches before (_,a,b,c) after opcode = behave (mkInstruction opcode a b c) before == after

solve1 = length . filter (>= 3) . fmap (length . filter id . (\(b,i,a) -> matches b i a <$> opcodes)) . parseData

-- how many samples in your puzzle input behave like three or more opcodes
solution1 = solve1 <$> input
-- 542

opcodeCandidates :: [String] -> [[(Int, Opcode)]]
opcodeCandidates = nub . sortOn length . fmap (catMaybes . (\(b,i@(opcode,_,_,_),a) -> (\oc -> if matches b i a oc then Just (opcode,oc) else Nothing) <$> opcodes)) . parseData

bar (((i,o):_):remaining,res) = (sortOn length $ filter (not . null) $ fmap (filter ((/= o) . snd) . filter ((/= i) . fst)) remaining, (i,o) : res)
bar ([],res) = ([],res)

matchOpcodes :: [String] -> [(Int, Opcode)]
matchOpcodes = snd . head . filter (null . fst) . iterate' bar . (,[]) . opcodeCandidates

solve2 = flip S.index 0 . fmap value . foldl (flip behave) (S.fromList $ fmap Register [0,0,0,0]) . (\(matching,is) -> fmap (\(oc,a,b,c) -> mkInstruction (snd $ head $ filter ((== oc) . fst) matching) a b c) is) . bimap matchOpcodes (fmap instr)

solution2 = solve2 <$> do i1 <- input; i2 <- input2; pure (i1,i2)
-- 575