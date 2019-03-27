{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Day19 where
import           Control.Arrow ((&&&), second)
import           Data.Bifunctor (bimap)
import           Data.Bits ((.&.), (.|.))
import           Data.Function (on)
import           Data.List (nub, nubBy, sortOn, iterate')
import           Data.List.Split (chunksOf)
import           Data.Maybe
       (fromJust, isJust, isNothing, listToMaybe, maybeToList, catMaybes,
        fromMaybe, mapMaybe)
import           Data.Ord (comparing)
import qualified Data.Sequence as S
import           Data.Sequence (Seq)
import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed (Vector)
import           Numeric.Natural
import           Prelude hiding ((!!))
import           Text.Parsec (parse,many,many1,optional,(<|>))
import           Text.Parsec.Char (char,space,string,letter,digit,anyChar)
import           Text.Parsec.Combinator (between,sepBy)
import           Text.ParserCombinators.Parsec.Number (int,nat)


input = lines <$> readFile  "input/input19.txt"

ipP = string "#ip " *> nat
instrP = (,,,) <$> (toOpcode <$> many letter) <*> (char ' ' *> int) <*> (char ' ' *> int) <*> (char ' ' *> int)

toOpcode "addr" = AddR
toOpcode "addi" = AddI
toOpcode "mulr" = MulR
toOpcode "muli" = MulI
toOpcode "banr" = BanR
toOpcode "bani" = BanI
toOpcode "borr" = BorR
toOpcode "bori" = BorI
toOpcode "setr" = SetR
toOpcode "seti" = SetI
toOpcode "gtir" = GtIR
toOpcode "gtri" = GtRI
toOpcode "gtrr" = GtRR
toOpcode "eqir" = EqIR
toOpcode "eqri" = EqRI
toOpcode "eqrr" = EqRR

ps p = either undefined id . parse p ""
ip = ps ipP
instr :: String -> (Opcode,Int,Int,Int)
instr = ps instrP

parseData = bimap ip (fmap instr) . (head &&& tail)

type Register = Int

data Input = Reg Natural | Val Int

data Opcode = AddR | AddI |
              MulR | MulI |
              BanR | BanI |
              BorR | BorI |
              SetR | SetI |
              GtIR | GtRI | GtRR |
              EqIR | EqRI | EqRR
    deriving (Show,Eq,Ord)

data Instruction = Instruction {
    opcode :: Opcode,
    a :: Input,
    b :: Input,
    c :: Input
}

type Registers = Vector Register
type Instructions = Seq Instruction

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

mkInstructions = S.fromList . fmap (\(oc,a,b,c) -> mkInstruction oc a b c)


-- Process

(!) :: Registers -> Natural -> Int
(!) regs i = regs V.! fromIntegral i

(!!) :: Instructions -> Natural -> Instruction
(!!) instrs i = S.index instrs (fromIntegral i)

update' :: Input -> Int -> Registers -> Registers
update' (Reg i) val = (`V.unsafeUpd` [(fromIntegral i, val)])

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

type IP = Natural

process :: Input -> Seq Instruction -> IP -> Registers -> (IP,Registers)
process ipReg@(Reg r) instructions ip registers = let
    instr = instructions !! ip
    foo = update' ipReg (fromIntegral ip) registers
    bar = behave instr foo
  in
    (fromIntegral $ (bar ! r) + 1, bar)

solv regs ipReg instructions = iterate' (uncurry $ process ipReg instructions) (0,regs)

continues numberOfInstructions = (&&) <$> (>= 0) <*> (< numberOfInstructions)

getIP = fromIntegral . S.length . snd

firstStateAfterHalt regs = (\f -> head . dropWhile (f . fst)) <$> continues . getIP <*> uncurry (solv regs)

solve regs = (V.! 0) . snd . firstStateAfterHalt (V.fromList regs) . bimap Reg mkInstructions . parseData

-- What value is left in register 0
solution1 = solve [0,0,0,0,0,0] <$> input
-- 948

factors n = [x | x <- [1..n], n `mod` x == 0]

-- seems to be factorization, and the number to factorize is initialized to reg 4
solve2 = sum . factors . (! 4) . snd . head . drop 100 . uncurry (solv (V.fromList [1,0,0,0,0,0])) . bimap Reg mkInstructions . parseData

-- this time, register 0 started with the value 1. What value is left in register 0
solution2 = solve2 <$> input
-- 10695960