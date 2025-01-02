{-# LANGUAGE MagicHash #-}
module Blog where

import Universum ((...))
import Control.Monad (ap)
import Data.Function (on, (&))
import Control.Monad (join)
import Control.Category ((>>>))

-- idiot
i, i#, i##, i### :: a -> a
i x = x
i# = s k k
i## = id
i### = undefined--s k s

-- idiot once removed
a, a#, i_ :: (a -> b) -> a -> b
a x y = x y
a# = ($)
i_ = b i

-- idiot twice removed
i__, i__# :: (a -> b -> c) -> a -> b -> c
i__ x y z = x y z
i__# = b i_

i___, i___# :: (a -> b -> c -> d) -> a -> b -> c -> d
i___ x y z w = x y z w
i___# = b i__

i____, i____# :: (a -> b -> c -> d -> e) -> a -> b -> c -> d -> e
i____ x y z w v = x y z w v
i____# = b i___

-- kestrel
k, k# :: a -> b -> a
k x y = x
k# = const

-- kite
ki, ki# :: a -> b -> b
ki x y = y
ki# = const id

-- bluebird
b, b#, b##, b### :: (b -> c) -> (a -> b) -> a -> c
b x y z = x (y z)
b# = q t (q q)
b## = s (k s) k
b### = (.)

-- blackbird
b1, b1#, b1## :: (c -> d) -> (a -> b -> c) -> a -> b -> d
b1 x y z w = x (y z w)
b1# = b b b
b1## = (.) . (.)

-- bunting
b2, b2# :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
b2 x y z w v = x (y z w v)
b2# = (.) . (.) . (.)

-- becard
b3, b3# :: (c -> d) -> (b -> c) -> (a -> b) -> a -> d
b3 x y z w = x (y (z w))
b3# = (. (.)) . (.) . (.)

-- starling
s, s#, s##, s###, s#### :: (a -> b -> c) -> (a -> b) -> a -> c
s x y z = x z (y z)
s# = b (b w) (b b c)
s## = b w_ g
s### = w__ g
s#### = ap

missing, missing# :: (b -> a -> c) -> (a -> b) -> a -> c
missing x y z = x (y z) z
missing# = flip (flip . (ap .) . (.)) id

-- phoenix / starling prime
pho, pho# :: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
pho x y z w = x (y w) (z w)
pho# = (ap .) . (.)

-- dove / bluebird prime
d, b', b'# :: (a -> c -> d) -> a -> (b -> c) -> b -> d
d x y z w = x y (z w)
b' = b b
b'# = ((.) .)

-- cardinal prime
c', c'# :: (c -> a -> d) -> (b -> c) -> a -> b -> d
c' x y z w = x (y w) z
c'# = (flip .) . (.)

-- eagle
e, e#, e##, e### :: (a -> d -> e) -> a -> (b -> c -> d) -> b -> c -> e
e x y z w v = x y (z w v)
e# = b (b b b)
e## = b b1
e### = (((.) . (.)) .)

-- dickcissel
d1, d1#, d1##, d1### :: (a -> b -> d -> e) -> a -> b -> (c -> d) -> c -> e
d1 x y z w v = x y z (w v)
d1# = b d
d1## = b (b b)
d1### = (((.) .) .)

-- psi
psi, psi#, psi## :: (b -> b -> c) -> (a -> b) -> a -> a -> c
psi x y z w = x (y z) (y w)
psi# = on
psi## = join . ((flip . ((.) .)) .) . (.)

-- dovekie
d2, d2# :: (c -> d -> e) -> (a -> c) -> a -> (b -> d) -> b -> e
d2 x y z w v = x (y z) (w v)
d2# = (((.) .) .) . (.)

-- bald eagle
e_, e_#, e_##, e_###, e_#### :: (e -> f -> g) -> (a -> b -> e) -> a -> b -> (c -> d -> f) -> c -> d -> g
e_ x y1 y2 y3 z1 z2 z3 = x (y1 y2 y3) (z1 z2 z3)
e_# = e e
e_## = b(b b b)(b(b b b))
e_### = (b b1)(b b1)
e_#### = (((((.) . (.)) .) .) .) . (.) . (.)

-- warbler
w, w#, w##, w###, w####, w#####, w######, w#######, w########, w######### :: (a -> a -> b) -> a -> b
w x y = x y y
w# = c (b m r)
w## = c w'
w### = b (t w') r
w#### = c (h r)
w##### = r (h r) r
w###### = c (s r r)
w####### = c (s (c c) (c c))
w######## = s t
w######### = flip ap id

-- warbler once removed
w_, w_#, w_## :: (a -> b -> b -> c) -> a -> b -> c
w_ x y z = x y z z
w_# = b w
w_## = flip flip id . (ap .)

-- warbler twice removed
w__, w__#, w__##, w__### :: (a -> b -> c -> c -> d) -> a -> b -> c -> d
w__ x y z w = x y z w w
w__# = b (b w)
w__## = b w_
w__### = flip flip id . ((flip . (ap .)) .)

-- hummingbird
h, h#, h##, h###, h#### :: (a -> b -> a -> c) -> a -> b -> c
h x y z = x y z y
h# = b w (b c)
h## = w_ c_
h### = s r
h#### = flip (ap . (flip .)) id

-- jay
j, j# :: (a -> b -> b) -> a -> b -> a -> b
j x y z w = x y (x w z)
j# = ap (flip . ((.) .) . ((.) .)) flip

-- jalt
jalt, jalt# :: (a -> c) -> a -> b -> c
jalt x y z = x y
jalt# = (const .)

-- jalt prime
jalt', jalt'# :: (a -> b -> d) -> a -> b -> c -> d
jalt' x y z w = x y z
jalt'# = ((const .) .)

-- gamma
gamma, gamma# :: ((a -> b -> c) -> d -> e -> b) -> (a -> b -> c) -> (d -> a) -> d -> e -> c
gamma x y z w v = y (z w) (x y w v)
gamma# = ap (flip . (ap .) . (((.) .) .) . (.))

-- mockingbird
m, m#, m##, m###, m#### :: (b -> c) -> c
m x = x x
m# = o i
m## = w t
m### = s t t
m#### = s i i

-- double mockingbird
m2, m2# :: (a -> t -> c) -> a -> c
m2 x y = x y (x y)
m2# = b m

-- lark
l, l#, l##, l###, l####, l#####, l######, l####### :: (b -> c) -> (a -> b) -> c
l x y = x (y y)
l# = q m
l## = c b m
l### = r m b
l#### = b b t m b
l##### = b (t m) b
l###### = b w b
l####### = (. ap id id)

-- owl
o, o#, o##, o###, o####, o##### :: ((a -> b) -> a) -> (a -> b) -> b
o x y = y (x y)
o# = s i
o## = q q w
o### = b w q
o#### = b w (c b)
o##### = ap id

-- sage
sage, sage# :: (a -> a) -> a
sage x = x (sage x)
sage# = s l l

-- turing
u, u#, u##, u### :: (a -> (b -> c) -> b) -> (b -> c) -> c
u x y = y (x x y)
u# = l o
u## = l (s i)
u### = b w (l q)

-- goldfinch
g, g#, g## :: (b -> c -> d) -> (a -> c) -> a -> b -> d
g x y z w = x w (y z)
g# = b b c
g## = (.) . flip

-- thrush
t, t#, t##, t###, t#### :: a -> (a -> b) -> b
t x y = y x
t# = c i
t## = s (k (s i)) (s (k k) i)
t### = q3 i
t#### = (&)

-- cardinal
c, c#, c##, c###, c####, c##### :: (a -> b -> c) -> b -> a -> c
c x y z = x z y
c# = r r r
c## = b (t (b b t)) (b b t)
c### = q q (q t)
c#### = g g i i
c##### = flip

-- finch
f, f#, f##, f###, f#### :: b -> a -> (a -> b -> c) -> c
f x y z = z y x
f# = e t t e t
f## = b (t t) (b (b b b) t)
f### = c v
f#### = flip (flip . flip id)

-- robin
r, r#, r##, r### :: b -> (a -> b -> c) -> a -> c
r x y z = y z x
r# = b b t
r## = c c
r### = flip flip

-- vireo
v, v#, v##, v###, v#### :: a -> b -> (a -> b -> c) -> c
v x y z = z x y
v# = b c t
v## = c f
v### = c_ t
v#### = flip . flip id

-- robin once removed
r_, r_#, r_## :: (b -> c -> a -> d) -> a -> b -> c -> d
r_ x y z w = x z w y
r_# = c_ c_
r_## = flip . (flip .)

-- finch once removed
f_, f_#, f_## :: (c -> b -> a -> d) -> a -> b -> c -> d
f_ x y z w = x w z y
f_# = b c_ r_
f_## = flip . (flip .) . flip

-- cardinal once removed
c_, c_#, c_##, c_### :: (a -> c -> b -> d) -> a -> b -> c -> d
c_ x y z w = x y w z
c_# = b c
c_## = g r
c_### = (flip .)

-- vireo once removed
v_, v_#, v_## :: (b -> a -> b -> d) -> a -> b -> b -> d
v_ x y z w = x z y w
v_# = c_ f_
v_## = flip

-- robin twice removed
r__, r__#, r__## :: (a -> c -> d -> b -> e) -> a -> b -> c -> d -> e
r__ x y z w v = x y w v z
r__# = b r_
r__## = ((flip . (flip .)) .)

-- finch twice removed
f__, f__#, f__## :: (a -> d -> c -> b -> e) -> a -> b -> c -> d -> e
f__ x y z w v = x y v w z
f__# = b f_
f__## = ((flip . (flip .) . flip) .)

-- cardinal twice removed
c__, c__# :: (a -> b -> d -> c -> e) -> a -> b -> c -> d -> e
c__ x y z w v = x y z v w
c__# = b c_

-- vireo twice removed
v__, v__#, v__## :: (a -> c -> b -> c -> e) -> a -> b -> c -> c -> e
v__ x y z w v = x y v z w
v__# = b v_
v__## = (((flip .) . flip) .)

-- queer
q, q#, q##, q###, q####, q#####, q######, q#######, q########, q#########, q########## :: (a -> b) -> (b -> c) -> a -> c
q x y z = y (x z)
q# = c b
q## = r r r b
q### = r b r
q#### = b b t b r
q##### = b (t b) r
q###### = b (t b) (b b t)
q####### = g r q3
q######## = c_ q3
q######### = (>>>)
q########## = flip (.)

-- quixotic
q1, q1#, q1##, q1### :: (b -> c) -> a -> (a -> b) -> c
q1 x y z = x (z y)
q1# = b c b
q1## = c_ b
q1### = (. flip id) . (.)

-- quizzical
q2, q2#, q2##, q2###, q2#### :: a -> (b -> c) -> (a -> b) -> c
q2 x y z = y (z x)
q2# = r_ b
q2## = b c (b c) b
q2### = c (b c b)
q2#### = flip (.) . flip id

-- quirky
q3, q3#, q3##, q3### :: (a -> b) -> a -> (b -> c) -> c
q3 x y z = z (x y)
q3# = b t
q3## = g i
q3### = (flip id .)

-- quacky
q4, q4#, q4##, q4###, q4####, q4##### :: a -> (a -> b) -> (b -> c) -> c
q4 x y z = z (y x)
q4# = f_ b
q4## = c (b t)
q4### = q1 t
q4#### = c q3
q4##### = (flip id .) . flip id

-- converse warbler
w', w'#, w'##, w'###, w'####, w'##### :: a -> (a -> a -> b) -> b
w' x y = y x x
w'# = b m r
w'## = m2 r
w'### = b m (b b t)
w'#### = b (b m b) t
w'##### = h r
