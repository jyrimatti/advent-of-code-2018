{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections         #-}
module Day21 where

import           Control.Arrow                        (second, (&&&))
import           Data.Bifunctor                       (bimap)
import           Data.Bits                            ((.&.), (.|.))
import           Data.Either                          (fromLeft, isLeft,
                                                       isRight)
import           Data.Function                        (on)
import           Data.List                            (iterate')
import           Data.List.Split                      (chunksOf)
import           Data.Maybe                           (catMaybes, fromJust,
                                                       fromMaybe, isJust,
                                                       isNothing, listToMaybe,
                                                       mapMaybe, maybeToList)
import           Data.Ord                             (comparing)
import           Data.Set                             (Set, empty, insert,
                                                       member)
import qualified Data.Vector                          as VB
import qualified Data.Vector                          as V
import           Data.Vector                          (Vector)
import           Prelude                              hiding ((!!))
import           Text.Parsec                          (many, many1, optional,
                                                       parse, (<|>))
import           Text.Parsec.Char                     (anyChar, char, digit,
                                                       letter, space, string)
import           Text.Parsec.Combinator               (between, sepBy)
import           Text.ParserCombinators.Parsec.Number (int, nat)



input = lines <$> readFile  "input/input21.txt"

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
instr :: String -> (Opcode,Integer,Integer,Integer)
instr = ps instrP

parseData = bimap ip (fmap instr) . (head &&& tail)

type Register = Integer

--data Input = Reg Int | Val Int
type Input = Integer

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
type Instructions = VB.Vector Instruction

reg = id
val = id

mkInstruction AddR a b c = Instruction AddR (reg a) (reg b) (reg c)
mkInstruction AddI a b c = Instruction AddI (reg a) (val b) (reg c)
mkInstruction MulR a b c = Instruction MulR (reg a) (reg b) (reg c)
mkInstruction MulI a b c = Instruction MulI (reg a) (val b) (reg c)
mkInstruction BanR a b c = Instruction BanR (reg a) (reg b) (reg c)
mkInstruction BanI a b c = Instruction BanI (reg a) (val b) (reg c)
mkInstruction BorR a b c = Instruction BorR (reg a) (reg b) (reg c)
mkInstruction BorI a b c = Instruction BorI (reg a) (val b) (reg c)
mkInstruction SetR a b c = Instruction SetR (reg a) (val b) (reg c)
mkInstruction SetI a b c = Instruction SetI (val a) (val b) (reg c)
mkInstruction GtIR a b c = Instruction GtIR (val a) (reg b) (reg c)
mkInstruction GtRI a b c = Instruction GtRI (reg a) (val b) (reg c)
mkInstruction GtRR a b c = Instruction GtRR (reg a) (reg b) (reg c)
mkInstruction EqIR a b c = Instruction EqIR (val a) (reg b) (reg c)
mkInstruction EqRI a b c = Instruction EqRI (reg a) (val b) (reg c)
mkInstruction EqRR a b c = Instruction EqRR (reg a) (reg b) (reg c)

mkInstructions = VB.fromList . fmap (\(oc,a,b,c) -> mkInstruction oc a b c)


-- Process

update' :: Input -> Integer -> Registers -> Registers
update' i = flip V.unsafeUpd . (: []) . (fromIntegral i, ) . fromIntegral

behave :: Instruction -> Registers -> Registers
behave (Instruction AddR a b c) regs = update' c (   regs `V.unsafeIndex` fromIntegral a  +  regs `V.unsafeIndex` fromIntegral b              ) regs
behave (Instruction AddI a b c) regs = update' c (   regs `V.unsafeIndex` fromIntegral a  +                       b              ) regs
behave (Instruction MulR a b c) regs = update' c (   regs `V.unsafeIndex` fromIntegral a  *  regs `V.unsafeIndex` fromIntegral b              ) regs
behave (Instruction MulI a b c) regs = update' c (   regs `V.unsafeIndex` fromIntegral a  *                       b              ) regs
behave (Instruction BanR a b c) regs = update' c (   regs `V.unsafeIndex` fromIntegral a .&. regs `V.unsafeIndex` fromIntegral b              ) regs
behave (Instruction BanI a b c) regs = update' c (   regs `V.unsafeIndex` fromIntegral a .&.                      b              ) regs
behave (Instruction BorR a b c) regs = update' c (   regs `V.unsafeIndex` fromIntegral a .|. regs `V.unsafeIndex` fromIntegral b              ) regs
behave (Instruction BorI a b c) regs = update' c (   regs `V.unsafeIndex` fromIntegral a .|.                      b              ) regs
behave (Instruction SetR a _ c) regs = update' c (   regs `V.unsafeIndex` fromIntegral a                                         ) regs
behave (Instruction SetI a _ c) regs = update' c                          a                                           regs
behave (Instruction GtIR a b c) regs = update' c (if                      a  >  regs `V.unsafeIndex` fromIntegral b then 1 else 0) regs
behave (Instruction GtRI a b c) regs = update' c (if regs `V.unsafeIndex` fromIntegral a  >                       b then 1 else 0) regs
behave (Instruction GtRR a b c) regs = update' c (if regs `V.unsafeIndex` fromIntegral a  >  regs `V.unsafeIndex` fromIntegral b then 1 else 0) regs
behave (Instruction EqIR a b c) regs = update' c (if                      a ==  regs `V.unsafeIndex` fromIntegral b then 1 else 0) regs
behave (Instruction EqRI a b c) regs = update' c (if regs `V.unsafeIndex` fromIntegral a ==                       b then 1 else 0) regs
behave (Instruction EqRR a b c) regs = update' c (if regs `V.unsafeIndex` fromIntegral a ==  regs `V.unsafeIndex` fromIntegral b then 1 else 0) regs

type IP = Integer

process :: Input -> Instructions -> (IP,Registers) -> (IP,Registers)
process ipReg instructions = ((+1) . (`V.unsafeIndex` fromIntegral ipReg) &&& id) . (behave <$> (instructions `VB.unsafeIndex`) . fromIntegral . fst <*> uncurry (update' ipReg))

solv regs ipReg instructions = iterate' (process ipReg instructions) (0,regs)

solve1 = (V.! 5) . snd . head . dropWhile ((/= 28) . fst) . uncurry (solv (V.fromList [0,0,0,0,0,0])) . second mkInstructions . parseData

-- What is the lowest non-negative integer value for register 0 that causes the program to halt after executing the fewest instructions?
solution1 = solve1 <$> input
-- 2884703

solve2 = head . snd . last . takeWhile (not . null . snd) . tail . scanl (\(s,rs) r -> if r `member` s then (s,[]) else (insert r s,r:rs)) (empty,[]) . fmap ((V.! 5) . snd) . filter ((== 28) . fst) . uncurry (solv (V.fromList [0,0,0,0,0,0])) . second mkInstructions . parseData

-- What is the lowest non-negative integer value for register 0 that causes the program to halt after executing the most instructions?
solution2 = solve2 <$> input
-- 15400966

--continues numberOfInstructions = (&&) <$> (>= 0) <*> (< numberOfInstructions)
--numberOfInstructions = fromIntegral . length . snd
--solve regs = ((\f -> fmap (f . fst)) <$> continues . numberOfInstructions <*> uncurry (solv regs)) . second mkInstructions . parseData
--solution1 = fmap (\(i,xs) -> if all (head . snd) xs then VB.fromList [(i,[])] else xs) . zip [0..] . iterate' (fmap (second $ \xs -> if head xs then tail xs else xs)) . (\s -> (\r -> (r,solve (V.fromList [r,0,0,0,0,0]) s)) <$> VB.fromList [2884703]) <$> input

act6:: Integer -> [Integer]
act6 r5 = let
    r3 = r5 .|. 65536
  in
    act8 r3 733884

act8 :: Integer -> Integer -> [Integer]
act8 r3 r5 = let
    r1 = r3 .&. 255
    r5_2 = (((r5 + r1) .&. 16777215) * 65899) .&. 16777215
  in if r3 < 256 then r5_2 : act6 r5_2 else act8 (r3 `div` 256) r5_2

foo = head . snd . last . takeWhile (not . null . snd) $ tail $ scanl (\(s,rs) r -> if r `member` s then (s,[]) else (insert r s,r:rs)) (empty,[]) $ act6 0