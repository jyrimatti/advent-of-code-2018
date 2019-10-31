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

oN = on
infixl 5 `oN`

-- some variations of "Applicative Functor of functions"

(<*<) :: (a -> b) -> a -> b
(<*<) = ($)
infixl 4 <*<

(<$>>) :: (a -> b -> e) -> (d -> a) -> (c -> b) -> d -> c -> e
(<$>>) f f1 f2 a b = f (f1 a) (f2 b)
infixl 4 <$>>

(<$>>>) :: (a -> b -> c -> g) -> (d -> a) -> (e -> b) -> (f -> c) -> d -> e -> f -> g
(<$>>>) f f1 f2 f3 a b c = f (f1 a) (f2 b) (f3 c)
infixl 4 <$>>>

(<$$>>) :: (a -> b -> e) -> (d -> c -> a) -> (d -> c -> b) -> d -> c -> e
(<$$>>) f f1 f2 a b = f (f1 a b) (f2 a b)
infixl 4 <$$>>

(<$$>>>) :: (a -> b -> c -> f) -> (d -> e -> a) -> (d -> e -> b) -> (d -> e -> c) -> d -> e -> f
(<$$>>>) f f1 f2 f3 a b = f (f1 a b) (f2 a b) (f3 a b)
infixl 4 <$$>>>

(<$$>>>>) :: (a -> b -> c -> d -> g) -> (e -> f -> a) -> (e -> f -> b) -> (e -> f -> c) -> (e -> f -> d) -> e -> f -> g
(<$$>>>>) f f1 f2 f3 f4 a b = f (f1 a b) (f2 a b) (f3 a b) (f4 a b)
infixl 4 <$$>>>>