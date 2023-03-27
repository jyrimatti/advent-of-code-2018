{-# LANGUAGE FlexibleContexts #-}
module Util where

import Data.FoldApp (FoldrApp, foldrApp)
import Data.Function                  ( on, (&) )
import Data.Profunctor   (Profunctor, dimap)
import Universum.VarArg ((...))
import Control.Arrow
import Control.Category (Category)

-- some additions:

singleton :: a -> [a]
singleton = (: [])

triple :: a -> (a, a, a)
triple = (,,) <$> id <*> id <*> id

flip2 :: (a -> b -> c -> d) -> a -> c -> b -> d
flip2 f a b c = f a c b

const2 :: a -> b -> c -> a
const2 = const . const

const3 :: a -> b -> c -> d -> a
const3 = const . const . const

const4 :: a -> b -> c -> d -> e -> a
const4 = const . const . const . const

const5 :: a -> b -> c -> d -> e -> f -> a
const5 = const . const . const . const . const

compose3 :: (c -> d) -> (b -> c) -> (a -> b) -> a -> d
compose3 f g h = f . g . h

compose4 :: (d -> e) -> (c -> d) -> (b -> c) -> (a -> b) -> a -> e
compose4 f g h i = f . g . h . i

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (w, x, y, z) = f w x y z

both3 :: (t -> c) -> (t, t, t) -> (c, c, c)
both3 f (a,b,c) = (f a, f b, f c)

argDrop :: a -> b -> a
argDrop = const

arg2 a b = b
arg3 a b c = c
arg31 a b c = a
arg32 a b c = b
arg33 a b c = c
arg41 a b c d = a
arg42 a b c d = b
arg43 a b c d = c
arg44 a b c d = d
arg51 a b c d e = a
arg52 a b c d e = b
arg53 a b c d e = c
arg54 a b c d e = d
arg55 a b c d e = e

fst4 :: (a, b, c, d) -> a
fst4 (a,b,c,d) = a

snd4 :: (a, b, c, d) -> b
snd4 (a,b,c,d) = b

thd4 :: (a, b, c, d) -> c
thd4 (a,b,c,d) = c

fth4 :: (a, b, c, d) -> d
fth4 (a,b,c,d) = d



onn :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
onn f g = f <&>>> g <*< g <*< g
infixl 0 `onn`

onnn :: (b -> b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> a -> c
onnn f g = f <&>>>> g <*< g <*< g <*< g
infixl 0 `onnn`

-- f2 `on` f1
-- ==
-- f2 <&>> f1 <*< f1

-- f3 `on3` f1
-- ==
-- f3 <&>>> f1 <*< f1 <*< f1



-- Data.FoldApp has this, but does not export it...
anyOf :: FoldrApp Bool Bool f => f
anyOf = foldrApp (||) False


-- variation of ($) with the same fixity as the following operators
(<*<) :: (a -> b) -> a -> b
(<*<) = ($)
infixl 4 <*<



-- some variations of "Applicative Functor of functions"



-- 1) each argument goes through its own function ("split") before the combining function

--          (s -> t -> z)
--  a - f1 -/    /
--  b - f2 -----/
(<&>>) :: (s -> t -> z) -> (a -> s) -> (b -> t) -> a -> b -> z
(<&>>) f f1 f2 a b = f (f1 a) (f2 b)
infixl 4 <&>>

--          (s -> t -> u -> z)
--  a - f1 -/    /    /
--  b - f2 -----/    /
--  c - f3 ---------/
(<&>>>) :: (s -> t -> u -> z) -> (a -> s) -> (b -> t) -> (c -> u) -> a -> b -> c -> z
(<&>>>) f f1 f2 f3 a b c = f (f1 a) (f2 b) (f3 c)
infixl 4 <&>>>

--          (s -> t -> u -> v -> z)
--  a - f1 -/    /    /    /
--  b - f2 -----/    /    /
--  c - f3 ---------/    /
--  d - f4 -------------/
(<&>>>>) :: (s -> t -> u -> v -> z) -> (a -> s) -> (b -> t) -> (c -> u) -> (d -> v) -> a -> b -> c -> d -> z
(<&>>>>) f f1 f2 f3 f4 a b c d = f (f1 a) (f2 b) (f3 c) (f4 d)
infixl 4 <&>>>>

--          (s -> t -> u -> v -> w -> z)
--  a - f1 -/    /    /    /    /
--  b - f2 -----/    /    /    /
--  c - f3 ---------/    /    /
--  d - f4 -------------/    /
--  e - f5 -----------------/
(<&>>>>>) :: (s -> t -> u -> v -> w -> z) -> (a -> s) -> (b -> t) -> (c -> u) -> (d -> v) -> (e -> w) -> a -> b -> c -> d -> e -> z
(<&>>>>>) f f1 f2 f3 f4 f5 a b c d e = f (f1 a) (f2 b) (f3 c) (f4 d) (f5 e)
infixl 4 <&>>>>>



-- This any better? Naah...

fooo2 :: (s -> t -> z) -> (a -> s) -> (b -> t) -> a -> b -> z
fooo2 f f1 f2 = f <€> f1 <€>> f2

baar2 :: (s -> t -> u -> z) -> (a -> s) -> (b -> t) -> (c -> u) -> a -> b -> c -> z
baar2 f f1 f2 f3 = f <€> f1 <€>> f2 <€>>> f3

baar3 :: (s -> t -> u -> v -> z) -> (a -> s) -> (b -> t) -> (c -> u) -> (d -> v) -> a -> b -> c -> d -> z
baar3 f f1 f2 f3 f4 = f <€> f1 <€>> f2 <€>>> f3 <€>>>> f4


(<€>) :: (b -> c) -> (a -> b) -> a -> c
x <€> f = x . f
infixr 2 <€>

(<€>>) :: (a -> b -> c) -> (a1 -> b) -> a -> a1 -> c
x <€>> f2 = x >>> (<<< f2)
infixl 1 <€>>

(<€>>>) :: (a -> a1 -> b -> c) -> (a2 -> b) -> a -> a1 -> a2 -> c
x <€>>> f3 = x >>> ((<<< f3) <<<)
infixl 1 <€>>>

(<€>>>>) :: (a -> a1 -> a2 -> b -> c) -> (a3 -> b) -> a -> a1 -> a2 -> a3 -> c
x <€>>>> f4 = x >>> (((<<< f4) .) .)
infixl 1 <€>>>>




-- 2) all arguments go through all functions ("replicate") before the combining function

-- "Amount of dollars" indicates amount of arguments.
-- "Amount of ending >" indicates amount of functions.

--       (s -> z)
--       /
--     f1
--  a -/
--(<$>) :: (s -> z) -> (a -> s) -> a -> z
-- This is just `fmap` for functions

--       (s -> t -> z)
--       /    /
--     f1   f2
--  a -/----/
--(<$>>) :: (s -> t -> z) -> (a -> s) -> (a -> t) -> a -> z
-- This is just: (s -> t -> z) <$> (a -> s) <*> (a -> t) 

-- and so on.


--        (s -> t -> z)
--        /    /
--      f1   f2
--  a -//---//
--  b -/----/
(<$$>>) :: (s -> t -> z) -> (a -> b -> s) -> (a -> b -> t) -> a -> b -> z
(<$$>>) f f1 f2 a b = f (f1 a b) (f2 a b)
infixl 4 <$$>>

--       (s -> t -> u -> z)
--       /    /    /
--      f1   f2   f3
--  a -//---//---//
--  b -/----/----/
(<$$>>>) :: (s -> t -> u -> z) -> (a -> b -> s) -> (a -> b -> t) -> (a -> b -> u) -> a -> b -> z
(<$$>>>) f f1 f2 f3 a b = f (f1 a b) (f2 a b) (f3 a b)
infixl 4 <$$>>>

--       (s -> t -> u -> v -> z)
--       /    /    /    /
--      f1   f2   f3   f4
--  a -//---//---//---//
--  b -/----/----/----/
(<$$>>>>) :: (s -> t -> u -> v -> z) -> (a -> b -> s) -> (a -> b -> t) -> (a -> b -> u) -> (a -> b -> v) -> a -> b -> z
(<$$>>>>) f f1 f2 f3 f4 a b = f (f1 a b) (f2 a b) (f3 a b) (f4 a b)
infixl 4 <$$>>>>

--        /- f1 -\
--  a -\ //- f2 -\\
--      X--- f3 - f - z
--  b -/ \\- f4 -//
--        \- f5 -/
(<$$>>>>>) :: (s -> t -> u -> v -> w -> z) -> (a -> b -> s) -> (a -> b -> t) -> (a -> b -> u) -> (a -> b -> v) -> (a -> b -> w) -> a -> b -> z
(<$$>>>>>) f f1 f2 f3 f4 f5 a b = f (f1 a b) (f2 a b) (f3 a b) (f4 a b) (f5 a b)
infixl 4 <$$>>>>>

--        /-- f1 --\
--  a -\ //-- f2 --\\
--      X---- f3 -- f - z
--  b -/ \\\- f4 -///
--        \\- f5 -//
--         \- f6 -/
(<$$>>>>>>) :: (s -> t -> u -> v -> w -> x -> z) -> (a -> b -> s) -> (a -> b -> t) -> (a -> b -> u) -> (a -> b -> v) -> (a -> b -> w) -> (a -> b -> x) -> a -> b -> z
(<$$>>>>>>) f f1 f2 f3 f4 f5 f6 a b = f (f1 a b) (f2 a b) (f3 a b) (f4 a b) (f5 a b) (f6 a b)
infixl 4 <$$>>>>>>


--  a -\
--      \ /- f1 -\
--  b ---X-- f2 - f - z
--      /
--  c -/
(<$$$>>) :: (s -> t -> z) -> (a -> b -> c -> s) -> (a -> b -> c -> t) -> a -> b -> c -> z
(<$$$>>) f f1 f2 a b c = f (f1 a b c) (f2 a b c)
infixl 4 <$$$>>

--  a -\
--      \ /- f1 -\
--  b ---X-- f2 - f - z
--      / \- f3 -/
--  c -/
(<$$$>>>) :: (s -> t -> u -> z) -> (a -> b -> c -> s) -> (a -> b -> c -> t) -> (a -> b -> c -> u) -> a -> b -> c -> z
(<$$$>>>) f f1 f2 f3 a b c = f (f1 a b c) (f2 a b c) (f3 a b c)
infixl 4 <$$$>>>

(<$$$>>>>) :: (s -> t -> u -> v -> z) -> (a -> b -> c -> s) -> (a -> b -> c -> t) -> (a -> b -> c -> u) -> (a -> b -> c -> v) -> a -> b -> c -> z
(<$$$>>>>) f f1 f2 f3 f4 a b c = f (f1 a b c) (f2 a b c) (f3 a b c) (f4 a b c)
infixl 4 <$$$>>>>

(<$$$>>>>>) :: (s -> t -> u -> v -> w -> z) -> (a -> b -> c -> s) -> (a -> b -> c -> t) -> (a -> b -> c -> u) -> (a -> b -> c -> v) -> (a -> b -> c -> w) -> a -> b -> c -> z
(<$$$>>>>>) f f1 f2 f3 f4 f5 a b c = f (f1 a b c) (f2 a b c) (f3 a b c) (f4 a b c) (f5 a b c)
infixl 4 <$$$>>>>>


(<$$$$>>) :: (s -> t -> z) -> (a -> b -> c -> d -> s) -> (a -> b -> c -> d -> t) -> a -> b -> c -> d -> z
(<$$$$>>) f f1 f2 a b c d = f (f1 a b c d) (f2 a b c d)
infixl 4 <$$$$>>

(<$$$$>>>) :: (s -> t -> u -> z) -> (a -> b -> c -> d -> s) -> (a -> b -> c -> d -> t) -> (a -> b -> c -> d -> u) -> a -> b -> c -> d -> z
(<$$$$>>>) f f1 f2 f3 a b c d = f (f1 a b c d) (f2 a b c d) (f3 a b c d)
infixl 4 <$$$$>>>

(<$$$$>>>>) :: (s -> t -> u -> v -> z) -> (a -> b -> c -> d -> s) -> (a -> b -> c -> d -> t) -> (a -> b -> c -> d -> u) -> (a -> b -> c -> d -> v) -> a -> b -> c -> d -> z
(<$$$$>>>>) f f1 f2 f3 f4 a b c d = f (f1 a b c d) (f2 a b c d) (f3 a b c d) (f4 a b c d)
infixl 4 <$$$$>>>>

(<$$$$>>>>>) :: (s -> t -> u -> v -> w -> z) -> (a -> b -> c -> d -> s) -> (a -> b -> c -> d -> t) -> (a -> b -> c -> d -> u) -> (a -> b -> c -> d -> v) -> (a -> b -> c -> d -> w) -> a -> b -> c -> d -> z
(<$$$$>>>>>) f f1 f2 f3 f4 f5 a b c d = f (f1 a b c d) (f2 a b c d) (f3 a b c d) (f4 a b c d) (f5 a b c d)
infixl 4 <$$$$>>>>>


(<$$$$$>>) :: (s -> t -> z) -> (a -> b -> c -> d -> e -> s) -> (a -> b -> c -> d -> e -> t) -> a -> b -> c -> d -> e -> z
(<$$$$$>>) f f1 f2 a b c d e = f (f1 a b c d e) (f2 a b c d e)
infixl 4 <$$$$$>>

(<$$$$$>>>) :: (s -> t -> u -> z) -> (a -> b -> c -> d -> e -> s) -> (a -> b -> c -> d -> e -> t) -> (a -> b -> c -> d -> e -> u) -> a -> b -> c -> d -> e -> z
(<$$$$$>>>) f f1 f2 f3 a b c d e = f (f1 a b c d e) (f2 a b c d e) (f3 a b c d e)
infixl 4 <$$$$$>>>