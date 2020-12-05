{-# LANGUAGE FlexibleContexts #-}
module Util where

import Data.FoldApp (FoldrApp, foldrApp)
import Data.Function                  ( on )
import Data.Profunctor   (Profunctor, dimap)
import Universum.VarArg ((...))

-- some Hackage additions:

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

-- must sometimes fix variadic type to make this pass type checker...
(...$$) :: (x -> y) -> (a -> b -> x) -> (a -> b -> y)
(...$$) = (...)
infixl 8 ...$$

(...$$$) :: (x -> y) -> (a -> b -> c -> x) -> (a -> b -> c -> y)
(...$$$) = (...)
infixl 8 ...$$$

(...$$$$) :: (x -> y) -> (a -> b -> c -> d -> x) -> (a -> b -> c -> d -> y)
(...$$$$) = (...)
infixl 8 ...$$$$

(...$$$$$) :: (x -> y) -> (a -> b -> c -> d -> e -> x) -> (a -> b -> c -> d -> e -> y)
(...$$$$$) = (...)
infixl 8 ...$$$$$

argDrop :: a -> b -> a
argDrop = const

arg1 a b = a
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

t41 (a,b,c,d) = a
t42 (a,b,c,d) = b
t43 (a,b,c,d) = c
t44 (a,b,c,d) = d

oN :: (b -> b -> c) -> (a -> b) -> a -> a -> c
oN = on
infixl 5 `oN`

oN3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
oN3 f g a b c = f (g a) (g b) (g c)
infixl 5 `oN3`

-- Data.FoldApp has this, but does not export it...
anyOf :: FoldrApp Bool Bool f => f
anyOf = foldrApp (||) False

-- some variations of "Applicative Functor of functions"

(<*<) :: (a -> b) -> a -> b
(<*<) = ($)
infixl 4 <*<


(<&>>) :: (s -> t -> z) -> (a -> s) -> (b -> t) -> a -> b -> z
(<&>>) f f1 f2 a b = f (f1 a) (f2 b)
infixl 4 <&>>

(<&>>>) :: (s -> t -> u -> z) -> (a -> s) -> (b -> t) -> (c -> u) -> a -> b -> c -> z
(<&>>>) f f1 f2 f3 a b c = f (f1 a) (f2 b) (f3 c)
infixl 4 <&>>>

(<&>>>>) :: (s -> t -> u -> v -> z) -> (a -> s) -> (b -> t) -> (c -> u) -> (d -> v) -> a -> b -> c -> d -> z
(<&>>>>) f f1 f2 f3 f4 a b c d = f (f1 a) (f2 b) (f3 c) (f4 d)
infixl 4 <&>>>>

(<&>>>>>) :: (s -> t -> u -> v -> w -> z) -> (a -> s) -> (b -> t) -> (c -> u) -> (d -> v) -> (e -> w) -> a -> b -> c -> d -> e -> z
(<&>>>>>) f f1 f2 f3 f4 f5 a b c d e = f (f1 a) (f2 b) (f3 c) (f4 d) (f5 e)
infixl 4 <&>>>>>


(<$$>>) :: (s -> t -> z) -> (a -> b -> s) -> (a -> b -> t) -> a -> b -> z
(<$$>>) f f1 f2 a b = f (f1 a b) (f2 a b)
infixl 4 <$$>>

(<$$>>>) :: (s -> t -> u -> z) -> (a -> b -> s) -> (a -> b -> t) -> (a -> b -> u) -> a -> b -> z
(<$$>>>) f f1 f2 f3 a b = f (f1 a b) (f2 a b) (f3 a b)
infixl 4 <$$>>>

(<$$>>>>) :: (s -> t -> u -> v -> z) -> (a -> b -> s) -> (a -> b -> t) -> (a -> b -> u) -> (a -> b -> v) -> a -> b -> z
(<$$>>>>) f f1 f2 f3 f4 a b = f (f1 a b) (f2 a b) (f3 a b) (f4 a b)
infixl 4 <$$>>>>

(<$$>>>>>) :: (s -> t -> u -> v -> w -> z) -> (a -> b -> s) -> (a -> b -> t) -> (a -> b -> u) -> (a -> b -> v) -> (a -> b -> w) -> a -> b -> z
(<$$>>>>>) f f1 f2 f3 f4 f5 a b = f (f1 a b) (f2 a b) (f3 a b) (f4 a b) (f5 a b)
infixl 4 <$$>>>>>

(<$$>>>>>>) :: (s -> t -> u -> v -> w -> x -> z) -> (a -> b -> s) -> (a -> b -> t) -> (a -> b -> u) -> (a -> b -> v) -> (a -> b -> w) -> (a -> b -> x) -> a -> b -> z
(<$$>>>>>>) f f1 f2 f3 f4 f5 f6 a b = f (f1 a b) (f2 a b) (f3 a b) (f4 a b) (f5 a b) (f6 a b)
infixl 4 <$$>>>>>>


(<$$$>>) :: (s -> t -> z) -> (a -> b -> c -> s) -> (a -> b -> c -> t) -> a -> b -> c -> z
(<$$$>>) f f1 f2 a b c = f (f1 a b c) (f2 a b c)
infixl 4 <$$$>>

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