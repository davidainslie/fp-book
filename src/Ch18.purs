-- Monads

module Ch18 where

import Data.Monoid.Additive
import Data.Tuple (Tuple(..))
import Data.Show (show)
import Effect (Effect)
import Effect.Console (log)
import Prelude
-- The compiler will probably suggest to use something like:
-- import Prelude (class Applicative, class Apply, class Functor, class Monoid, class Semigroup, class Semiring, Unit, discard, show, zero, (#), ($), (+), (<>))
  
{-
When you interface with the outside world, you’re dealing in the realm of Side-effects.
For example, when you read from a file or call a Web API, the value that’s returned from that Function will NOT be deterministic, i.e. it could be different for 2 different consecutive calls with the same inputs.
-}

{-
Type class Semigroupoid:
(Remember a Category consists of Objects and Morphisms)

class Semigroupoid a where
  compose :: ∀ b c d. a c d -> a b c -> a b d

infixr 9 compose as <<<

WTF?

Compose is really just:
compose :: ∀ a b c. (b -> c) -> (a -> b) -> (a -> c)

-- However, "->" is just sugar syntax for Function:
compose :: ∀ a b c. Function b c -> Function a b -> Function a c

-- Notice that Function appears to be just another data structure.
-- If it works for that data structure, why not generalize it for any data structure?
compose :: ∀ f a b c. f b c -> f a b -> f a c

-- Let's rename our generics, so that it starts with `a` rather than `f`:
compose :: ∀ a b c d. a c d -> a b c -> a b d

Low and behold... Semigroupoid

Let's just look at the Function instance of Semigroupoid:

instance semigroupoidFn :: Semigroupoid (->) where
  compose :: ∀ b c d. (c -> d) -> (b -> c) -> (b -> d)
  compose f g x = f (g x)

- Remember, (->), is the Type Operator for the Type Constructor, Function.
- Everywhere you see -> think of a but prefixed to see how this Type Signature relates to the Semigroupoid one.

-}

composeDebuggable :: ∀ b c d. (c -> Tuple String d) -> (b -> Tuple String c) -> (b -> Tuple String d)
composeDebuggable g f x =
  let Tuple s r = f x
      Tuple s' t = g r in
  Tuple (s <> s') t

f :: Int -> Tuple String Int
f x = Tuple "added 10\n" (x + 10)

g :: Int -> Tuple String Int
g y = Tuple "multiplied by 100\n" (y + 100)

h :: Int -> Tuple String Int
h = g `composeDebuggable` f

{-
The above is fine when all functions are side-effecting as expected by composeDebuggable.

How about composing Side-effect Function with Pure Ones?

Let’s write a Function that converts a Pure Function into a Side-effect one:
-}
makeFuncDebuggable :: ∀ a b. (a -> b) -> a -> Tuple String b
makeFuncDebuggable f x = Tuple "" (f x)

{-
makeFuncDebuggable will lift our Function into one that has a Side-effect,
i.e. it takes a Function that normally returns a Pure Value, b, and makes it return a Side-effect Value, Tuple String b.
-}

nse :: Int -> Int
nse x = x + 42

c :: Int -> Tuple String Int
c = makeFuncDebuggable nse `composeDebuggable` f

-- Function application?
f' :: Int -> Int
f' = (+) 10

g' :: Int -> Int
g' = (+) 100

y :: Int
y = 12345 # f' # g'
-- Rember # is like "apply from" whereas $ is "apply to"

-- We can solve this problem, like we did for composing, by writing a Side-effect version of #. Let’s call it applyDebuggable.

{-
Convert # i.e. applyFlipped to a version for side effects which we'll call applyDebuggable:

applyFlipped :: ∀ a b .a -> (a -> b) -> b
-}

applyDebuggable :: ∀ a b. Tuple String a -> (a -> Tuple String b) -> Tuple String b
applyDebuggable (Tuple s x) f =
  let Tuple s' r = f x in
    Tuple (s <> s') r

-- And so
y' :: Tuple String Int
y' = (12345 # f) `applyDebuggable` g

-- But we can improve this further using:
makeDebuggable :: ∀ a. a -> Tuple String a
makeDebuggable x = Tuple "" x

-- giving:
y'' :: Tuple String Int
y'' = makeDebuggable 12345 `applyDebuggable` f `applyDebuggable` g

{-
One step to being more generic:
-}
type Debuggable a = Tuple String a

composeDebuggable' :: ∀ b c d. (c -> Debuggable d) -> (b -> Debuggable c) -> (b -> Debuggable d)
composeDebuggable' g f x =
  let Tuple s r = f x
      Tuple s' t = g r in
  Tuple (s <> s') t

applyDebuggable' :: ∀ a b. Debuggable a -> (a -> Debuggable b) -> Debuggable b
applyDebuggable' (Tuple s x) f =
  let Tuple s' r = f x in
  Tuple (s <> s') r

makeFuncDebuggable' :: ∀ a b. (a -> b) -> a -> Debuggable b
makeFuncDebuggable' f x = Tuple "" (f x)

makeDebuggable' :: ∀ a. a -> Debuggable a
makeDebuggable' x = Tuple "" x

-- We can rewrite composeDebuggable to reuse applyDebuggable:
composeDebuggable'' :: ∀ b c d. (c -> Debuggable d) -> (b -> Debuggable c) -> (b -> Debuggable d)
composeDebuggable'' g f x =
  f x `applyDebuggable'` g

-- And the same for makeFuncDebuggable:
makeFuncDebuggable'' :: ∀ a b. (a -> b) -> a -> Debuggable b
makeFuncDebuggable'' f x = makeDebuggable' (f x)

------------------------------------------------

{-
Just like Debuggable, we could come up with Countable, where this time the first element of Tuple "counts" (accumulates) and the second polymorphic parameter "a" is the computational result:
type Countable a = Tuple Int a

We would end up implementing the same set of behaviour, where only the details are different - A perfect case to extract a Type Class, which we'll call SideEffect.

A first attempt (which we'll enhance):
class SideEffect s where
  makeSideEffect :: ∀ a. a -> s a -- Which is the same as "pure" from Applicative
  applySideEffect :: ∀ a b. s a -> (a -> s b) -> s b

we'll end up with:  
-}
class Applicative f <= SideEffect f where
  applySideEffect :: ∀ a b. f a -> (a -> f b) -> f b

-- We'll have to make Debuggable and Countable real Types (not just aliases) so that we can make them instances of SideEffect - let's do just Countable:
newtype Count = Count Int

derive newtype instance semiringCount :: Semiring Count -- This will allow us to use (+) next

instance semigroupCount :: Semigroup Count where
  append = (+)

newtype Countable a = Countable (Tuple Count a)
-- We use Count (essentially wrapping Int) instead of just an Int, because Int has multiple semigroups
-- Below, when we derive Apply (for Countable) it implicitly derives from the Apply instance of Tuple (as that is what is being wrapped),
-- and that instance has a Semigroup constraint on the first Tuple entry which would be Int in our case, but which Semigroup instance should be used for Int? Hence we have a newtype around Int to derive our own.

-- We'll also need a Monoid for Count, as the below Applicative for Countable which delegates to Tuple's instance, and the Tuple instance has a constrait of Monoid:
instance monoidCount :: Monoid Count where
  mempty = zero -- Where zero is from Semiring

-- To implement a SideEffect for Countable, we'll need an Applicative, which in turn needs an Apply, which in turn needs a Functor:
derive newtype instance functorCountable :: Functor Countable

derive newtype instance applyCountable :: Apply Countable

derive newtype instance applicativeCountable :: Applicative Countable

instance sideEffectCountable :: SideEffect Countable where
  applySideEffect (Countable (Tuple c x)) f =
    let Countable (Tuple c' r) = f x in
      Countable $ Tuple (c <> c') r -- Or we could have done c + c'

------------------------------------------------

test :: Effect Unit
test = do
  log $ show y  
  log $ show y''