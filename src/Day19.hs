{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections         #-}
module Day19 where
import           Control.Applicative.Combinators (count, between, sepBy, optional)
import           Control.Arrow                        (second, (&&&))
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
import qualified Data.Vector.Unboxed                  as V
import           Data.Vector.Unboxed                  (Vector)
import           Numeric.Natural
import           Text.Megaparsec            (Parsec, anySingle, many, optional,
                                             parseMaybe, try, (<|>))
import           Text.Megaparsec.Char       (char, letterChar, space, string)
import           Text.Megaparsec.Char.Lexer (decimal, signed)
import           Universum.VarArg ((...))
import           Util

input :: IO [String]
input = lines <$> readFile  "input/input19.txt"

type Parser = Parsec () String

ipP :: Parser Natural
ipP = string "#ip " *> decimal

instrP :: Parser (Opcode,Int,Int,Int)
instrP = (,,,) <$> (toOpcode <$> many letterChar) <*> (char ' ' *> decimal) <*> (char ' ' *> decimal) <*> (char ' ' *> decimal)

toOpcode :: String -> Opcode
toOpcode = 
    if' <$> (== "addr") <*> const AddR <*> (
    if' <$> (== "addi") <*> const AddI <*> (
    if' <$> (== "mulr") <*> const MulR <*> (
    if' <$> (== "muli") <*> const MulI <*> (
    if' <$> (== "banr") <*> const BanR <*> (
    if' <$> (== "bani") <*> const BanI <*> (
    if' <$> (== "borr") <*> const BorR <*> (
    if' <$> (== "bori") <*> const BorI <*> (
    if' <$> (== "setr") <*> const SetR <*> (
    if' <$> (== "seti") <*> const SetI <*> (
    if' <$> (== "gtir") <*> const GtIR <*> (
    if' <$> (== "gtri") <*> const GtRI <*> (
    if' <$> (== "gtrr") <*> const GtRR <*> (
    if' <$> (== "eqir") <*> const EqIR <*> (
    if' <$> (== "eqri") <*> const EqRI <*> (
    if' <$> (== "eqrr") <*> const EqRR <*> undefined)))))))))))))))

ps :: Parser a -> String -> a
ps = fromJust ... parseMaybe

ip :: String -> Natural
ip = ps ipP

instr :: String -> (Opcode,Int,Int,Int)
instr = ps instrP

parseData :: [String] -> (Natural, [(Opcode, Int, Int, Int)])
parseData = bimap ip (fmap instr) . (head &&& tail)

type Register = Int

data Input = Reg { val :: Natural } | Val Int

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

reg :: Int -> Input
reg = Reg . fromIntegral

-- not gonna make these point-free...
mkInstruction :: Opcode -> Int -> Int -> Int -> Instruction
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

mkInstructions :: [(Opcode, Int, Int, Int)] -> Seq Instruction
mkInstructions = S.fromList . fmap (uncurry4 mkInstruction)


-- Process

(!) :: Registers -> Natural -> Int
(!) = (V.!) <&>> id <*< fromIntegral

(!!!) :: Instructions -> Natural -> Instruction
(!!!) = S.index <&>> id <*< fromIntegral

update' :: Input -> Int -> Registers -> Registers
update' = flip V.unsafeUpd . singleton ... (,) <&>> fromIntegral . val <*< id

-- not gonna make these point-free...
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

bar :: Input -> Instructions -> Natural -> Registers -> Registers
bar = behave <$$$$>> ((!!!) <$$$$>> arg42 <*< arg43)
                 <*< (update' <$$$$>>> arg41 <*< fromIntegral ... arg43 <*< arg44)

process :: Input -> Seq Instruction -> IP -> Registers -> (IP,Registers)
process = ((,) <$$>> fromIntegral . succ ... (!) <*< arg1) <$$$$>> bar <*< val ... arg41

baz :: Input -> Seq Instruction -> (IP, Registers) -> (IP, Registers)
baz = uncurry ...$$ process

solv :: Registers -> Input -> Seq Instruction -> [(IP, Registers)]
solv = (...$$ baz) . flip iterate' . (0,)

continues :: IP -> IP -> Bool
continues = (&&) <$$>> (>= 0) ... arg2
                   <*< flip (<)

getIP :: (a, Seq a1) -> IP
getIP = fromIntegral . S.length . snd

firstStateAfterHalt :: Registers -> (Input, Seq Instruction) -> (IP, Registers)
firstStateAfterHalt = head ... dropWhile . (. fst) <$$>> continues . getIP ... arg2
                                                     <*< uncurry . solv

solve :: [Register] -> [String] -> Register
solve = (V.! 0) . snd ... firstStateAfterHalt <&>> V.fromList
                                               <*< bimap Reg mkInstructions . parseData

-- What value is left in register 0
solution1 = solve [0,0,0,0,0,0] <$> input
-- 948

factors :: Int -> [Int]
factors = filter <$> (== 0) ... mod <*> enumFromTo 1

-- seems to be factorization, and the number to factorize is initialized to reg 4
solve2 :: [String] -> Int
solve2 = sum . factors . (! 4) . snd . (!! 100) . uncurry (solv (V.fromList [1,0,0,0,0,0])) . bimap Reg mkInstructions . parseData

-- this time, register 0 started with the value 1. What value is left in register 0
solution2 = solve2 <$> input
-- 10695960