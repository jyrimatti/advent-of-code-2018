{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections         #-}
module Day21 where

import           Control.Applicative.Combinators (between, count, optional,
                                                  sepBy)
import           Control.Conditional (if')
import           Control.Arrow                   (second, (&&&))
import           Data.Bifunctor                  (bimap)
import           Data.Bits                       ((.&.), (.|.))
import           Data.Either                     (fromLeft, isLeft, isRight)
import           Data.Function                   (on)
import           Data.List                       (iterate')
import           Data.List.Split                 (chunksOf)
import           Data.Maybe                      (catMaybes, fromJust,
                                                  fromMaybe, isJust, isNothing,
                                                  listToMaybe, mapMaybe,
                                                  maybeToList)
import           Data.Ord                        (comparing)
import           Data.Set                        (Set, empty, insert, member)
import qualified Data.Vector                     as VB
import qualified Data.Vector                     as V
import           Data.Vector                     (Vector)
import           Prelude                         hiding ((!!))
import           Text.Megaparsec                 (Parsec, anySingle, many,
                                                  optional, parseMaybe, try,
                                                  (<|>))
import           Text.Megaparsec.Char            (char, letterChar, space,
                                                  string)
import           Text.Megaparsec.Char.Lexer      (decimal, signed)
import           Universum.VarArg ((...))
import           Util



input :: IO [String]
input = lines <$> readFile  "input/input21.txt"

type Parser = Parsec () String

ipP :: Parser Integer
ipP = string "#ip " *> decimal

instrP :: Parser (Opcode,Integer,Integer,Integer)
instrP = (,,,) <$> (toOpcode <$> many letterChar) <*> (char ' ' *> decimal) <*> (char ' ' *> decimal) <*> (char ' ' *> decimal)

toOpcode :: String -> Opcode
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

ps :: Parser c -> String -> c
ps = fromJust ... parseMaybe

ip :: String -> Integer
ip = ps ipP

instr :: String -> (Opcode,Integer,Integer,Integer)
instr = ps instrP

parseData :: [String] -> (Integer, [(Opcode, Integer, Integer, Integer)])
parseData = bimap ip (fmap instr) . (head &&& tail)

type Register = Integer

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

mkInstructions :: [(Opcode, Input, Input, Input)] -> Vector Instruction
mkInstructions = VB.fromList . fmap (uncurry4 Instruction)


-- Process

update' :: Input -> Integer -> Registers -> Registers
update' = flip V.unsafeUpd . singleton ... (,) . fromIntegral

behave :: Instruction -> Registers -> Integer
behave (Instruction AddR a b c) regs =    regs `V.unsafeIndex` fromIntegral a  +  regs `V.unsafeIndex` fromIntegral b
behave (Instruction AddI a b c) regs =    regs `V.unsafeIndex` fromIntegral a  +                                    b
behave (Instruction MulR a b c) regs =    regs `V.unsafeIndex` fromIntegral a  *  regs `V.unsafeIndex` fromIntegral b
behave (Instruction MulI a b c) regs =    regs `V.unsafeIndex` fromIntegral a  *                                    b
behave (Instruction BanR a b c) regs =    regs `V.unsafeIndex` fromIntegral a .&. regs `V.unsafeIndex` fromIntegral b
behave (Instruction BanI a b c) regs =    regs `V.unsafeIndex` fromIntegral a .&.                                   b
behave (Instruction BorR a b c) regs =    regs `V.unsafeIndex` fromIntegral a .|. regs `V.unsafeIndex` fromIntegral b
behave (Instruction BorI a b c) regs =    regs `V.unsafeIndex` fromIntegral a .|.                                   b
behave (Instruction SetR a _ c) regs =    regs `V.unsafeIndex` fromIntegral a
behave (Instruction SetI a _ c) regs =                                      a
behave (Instruction GtIR a b c) regs = if                                   a  >  regs `V.unsafeIndex` fromIntegral b then 1 else 0
behave (Instruction GtRI a b c) regs = if regs `V.unsafeIndex` fromIntegral a  >                                    b then 1 else 0
behave (Instruction GtRR a b c) regs = if regs `V.unsafeIndex` fromIntegral a  >  regs `V.unsafeIndex` fromIntegral b then 1 else 0
behave (Instruction EqIR a b c) regs = if                                   a ==  regs `V.unsafeIndex` fromIntegral b then 1 else 0
behave (Instruction EqRI a b c) regs = if regs `V.unsafeIndex` fromIntegral a ==                                    b then 1 else 0
behave (Instruction EqRR a b c) regs = if regs `V.unsafeIndex` fromIntegral a ==  regs `V.unsafeIndex` fromIntegral b then 1 else 0

behave2 :: Instruction -> Registers -> Registers
behave2 = update' <$$>>> c ... const <*< behave <*< arg2

type IP = Integer

process :: Input -> Instructions -> (IP,Registers) -> (IP,Registers)
process = (.) <$$>> ((,) <$$>> succ ... flip V.unsafeIndex . fromIntegral <*< arg2) ... const
                <*< (behave2 <$$$>> (. fromIntegral . fst) . VB.unsafeIndex ... arg2 <*< uncurry . update' ... const)

solv :: Registers -> Input -> Instructions -> [(IP, Registers)]
solv = iterate' <$$$>> const process <*< (0,) ... arg31

solve1 :: [String] -> Register
solve1 = (V.! 5) . snd . head . dropWhile ((/= 28) . fst) . uncurry (solv (V.fromList [0,0,0,0,0,0])) . second mkInstructions . parseData

-- What is the lowest non-negative integer value for register 0 that causes the program to halt after executing the fewest instructions?
solution1 = solve1 <$> input
-- 2884703

bar :: (Set Register, [Register]) -> Register -> (Set Register, [Register])
bar = if' <$$>>> (flip member <&>> fst <*< id)
             <*< (,[]) . fst ... const
             <*< ((,) <$$>> (flip insert <&>> fst <*< id) <*< (flip (:) <&>> snd <*< id))

solve2 :: [String] -> Register
solve2 = head . snd . last . takeWhile (not . null . snd) . tail . scanl bar (empty,[]) . fmap ((V.! 5) . snd) . filter ((== 28) . fst) . uncurry (solv (V.fromList [0,0,0,0,0,0])) . second mkInstructions . parseData

-- What is the lowest non-negative integer value for register 0 that causes the program to halt after executing the most instructions?
solution2 = solve2 <$> input
-- 15400966
