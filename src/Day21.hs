{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections         #-}
module Day21 where

import           Control.Applicative.Combinators (between, count, optional, sepBy)
import           Control.Conditional (if', bool)
import           Control.Arrow (second, (&&&))
import           Data.Bifunctor (bimap)
import           Data.Bits ((.&.), (.|.))
import           Data.Either (fromLeft, isLeft, isRight)
import           Data.Function (on)
import           Data.List (iterate')
import           Data.List.Split (chunksOf)
import           Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import           Data.Ord (comparing)
import           Data.Set (Set, empty, insert, member)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as VB
import           Prelude hiding ((!!))
import           Text.Megaparsec (Parsec, anySingle, many, optional, parseMaybe, try, (<|>))
import           Text.Megaparsec.Char (char, letterChar, space, string)
import           Text.Megaparsec.Char.Lexer (decimal, signed)
import           Universum.VarArg ((...))
import           Util ((<$$$>>), (<$$>>), (<$$>>>), (<&>>), (<*<), arg2, arg31
                     , singleton, uncurry4, (<&), (&>), (<$$$$$>>>), (<$$$$$>>)
                     , arg51, arg52, arg53, arg54, arg55, arg1, (<$$>>>>>))


input :: IO [String]
input = lines <$> readFile  "input/input21.txt"

type Parser = Parsec () String

ipP :: Parser Int
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

ps :: Parser c -> String -> c
ps = fromJust ... parseMaybe

ip :: String -> Int
ip = ps ipP

instr :: String -> (Opcode,Int,Int,Int)
instr = ps instrP

parseData :: [String] -> (Int, [(Opcode, Int, Int, Int)])
parseData = bimap ip (fmap instr) . (head &&& tail)

type Register = Int

type Input = Int

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

type Registers = V.Vector Register
type Instructions = VB.Vector Instruction

mkInstructions :: [(Opcode, Input, Input, Input)] -> VB.Vector Instruction
mkInstructions = VB.fromList . fmap (uncurry4 Instruction)


-- Process

update' :: Input -> Int -> Registers -> Registers
update' = flip V.unsafeUpd . singleton ... (,) . fromIntegral

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

getVal :: a -> a
getVal = id

_getReg :: (Opcode -> Input -> Input -> Input -> Registers -> Input) -> (Opcode -> Input -> Input -> Input -> Registers -> Int)
_getReg = ((V.!) <$$$$$>> _regs <*<)

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

type IP = Int

process :: Input -> Instructions -> (IP,Registers) -> (IP,Registers)
process = (.) <$$>> ((,) <$$>> succ ... flip (V.!) . fromIntegral <*< arg2) ... const
                <*< (behave <$$$>> (. fromIntegral . fst) . (VB.!) ... arg2 <*< uncurry . update' ... const)

solv :: Registers -> Input -> Instructions -> [(IP, Registers)]
solv = iterate' <$$$>> const process <*< (0,) ... arg31

solve1 :: [String] -> Register
solve1 = (V.! 5) . snd . head . dropWhile ((/= 28) . fst) . uncurry (solv (V.fromList [0,0,0,0,0,0])) . second mkInstructions . parseData

-- What is the lowest non-negative integer value for register 0 that causes the program to halt after executing the fewest instructions?
solution1 :: IO Register
solution1 = solve1 <$> input
-- 2884703

bar :: (Set Register, [Register]) -> Register -> (Set Register, [Register])
bar = if' <$$>>> fst &> flip member <& id
             <*< (,[]) . fst ... const
             <*< ((,) <$$>> fst &> flip insert <& id
                        <*< snd &> flip (:) <& id)

solve2 :: [String] -> Register
solve2 = head . snd . last . takeWhile (not . null . snd) . tail . scanl bar (empty,[]) . fmap ((V.! 5) . snd) . filter ((== 28) . fst) . uncurry (solv (V.fromList [0,0,0,0,0,0])) . second mkInstructions . parseData

-- What is the lowest non-negative integer value for register 0 that causes the program to halt after executing the most instructions?
solution2 :: IO Register
solution2 = solve2 <$> input
-- 15400966
