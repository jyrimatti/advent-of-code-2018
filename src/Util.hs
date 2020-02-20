module Util where

import Data.Function                  ( on )
import Data.Profunctor   (Profunctor, dimap)


-- some Hackage additions:

singleton = (: [])

const2 :: a -> b -> c -> a
const2 = const . const

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

oN = on
infixl 5 `oN`

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


(<$$>>) :: (s -> t -> z) -> (a -> b -> s) -> (a -> b -> t) -> a -> b -> z
(<$$>>) f f1 f2 a b = f (f1 a b) (f2 a b)
infixl 4 <$$>>

(<$$>>>) :: (s -> t -> u -> z) -> (a -> b -> s) -> (a -> b -> t) -> (a -> b -> u) -> a -> b -> z
(<$$>>>) f f1 f2 f3 a b = f (f1 a b) (f2 a b) (f3 a b)
infixl 4 <$$>>>

(<$$>>>>) :: (s -> t -> u -> v -> z) -> (a -> b -> s) -> (a -> b -> t) -> (a -> b -> u) -> (a -> b -> v) -> a -> b -> z
(<$$>>>>) f f1 f2 f3 f4 a b = f (f1 a b) (f2 a b) (f3 a b) (f4 a b)
infixl 4 <$$>>>>


(<$$$>>) :: (s -> t -> z) -> (a -> b -> c -> s) -> (a -> b -> c -> t) -> a -> b -> c -> z
(<$$$>>) f f1 f2 a b c = f (f1 a b c) (f2 a b c)
infixl 4 <$$$>>

(<$$$>>>) :: (s -> t -> u -> z) -> (a -> b -> c -> s) -> (a -> b -> c -> t) -> (a -> b -> c -> u) -> a -> b -> c -> z
(<$$$>>>) f f1 f2 f3 a b c = f (f1 a b c) (f2 a b c) (f3 a b c)
infixl 4 <$$$>>>

(<$$$>>>>) :: (s -> t -> u -> v -> z) -> (a -> b -> c -> s) -> (a -> b -> c -> t) -> (a -> b -> c -> u) -> (a -> b -> c -> v) -> a -> b -> c -> z
(<$$$>>>>) f f1 f2 f3 f4 a b c = f (f1 a b c) (f2 a b c) (f3 a b c) (f4 a b c)
infixl 4 <$$$>>>>


(<$$$$>>) :: (s -> t -> z) -> (a -> b -> c -> d -> s) -> (a -> b -> c -> d -> t) -> a -> b -> c -> d -> z
(<$$$$>>) f f1 f2 a b c d = f (f1 a b c d) (f2 a b c d)
infixl 4 <$$$$>>

(<$$$$>>>) :: (s -> t -> u -> z) -> (a -> b -> c -> d -> s) -> (a -> b -> c -> d -> t) -> (a -> b -> c -> d -> u) -> a -> b -> c -> d -> z
(<$$$$>>>) f f1 f2 f3 a b c d = f (f1 a b c d) (f2 a b c d) (f3 a b c d)
infixl 4 <$$$$>>>

(<$$$$>>>>) :: (s -> t -> u -> v -> z) -> (a -> b -> c -> d -> s) -> (a -> b -> c -> d -> t) -> (a -> b -> c -> d -> u) -> (a -> b -> c -> d -> v) -> a -> b -> c -> d -> z
(<$$$$>>>>) f f1 f2 f3 f4 a b c d = f (f1 a b c d) (f2 a b c d) (f3 a b c d) (f4 a b c d)
infixl 4 <$$$$>>>>