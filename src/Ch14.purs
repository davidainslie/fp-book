-- More Functors

{-
Functors are Homomorphisms.
Homo means same and morph means shape.
-}

module Ch14 where

import Data.Either
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude
  
----------------------------------------

-- Generic Product Types

newtype PV1 a = PV1 a

data PV2 a b = PV2 a b

instance functorPV1 :: Functor PV1 where
  map fn (PV1 x) = PV1 (fn x)

instance functorPV2 :: Functor (PV2 a) where
  map fn (PV2 x y) = PV2 x (fn y)

----------------------------------------

-- Generic Sum Types

data SV1 a = Nought | SV1 a

data SV2 a b = Nada | SV2A a | SV2B b

instance functorSV1 :: Functor SV1 where
  map _ Nought = Nought
  map fn (SV1 x) = SV1 (fn x)

instance functorSV2 :: Functor (SV2 a) where
  map _ Nada = Nada
  map _ (SV2A x) = SV2A x
  map fn (SV2B y) = SV2B (fn y)

----------------------------------------

-- Functors of Functions

newtype F1 a = F1 (Int -> a)

{-
instance functorF1 :: Functor F1 where
  map f (F1 g) = F1 (???)

f :: a -> b
g :: Int -> a

We can either compose:
- f with g OR
- g with f
but there is only one that compiles:
-}

instance functorF1 :: Functor F1 where
  map f (F1 g) = F1 (f <<< g)

-- Notice that when we map over Types that have Functions instead of Values, we don’t apply the Function to the Value.

-- If we swap the parameters of our newtype with function, then we run into an issue e.g.
newtype C1 a = C1 (a -> Int)

{-
instance functorC1 :: Functor C1 where
  map f (C1 g) = C1 (f <<< g) -- COMPILER ERROR!!
  OR
  map f (C1 g) = C1 (g <<< f) -- COMPILER ERROR!!


The issue is:
f :: (a -> b)
g :: (a -> Int)
f <<< g -- Type mismatch g <<< f -- Type mismatch 

A Functor's "map" maps OUTPUTS i.e. if we had
g :: (Int -> a)
the output "a" is threaded into "f".
But in the above, we now need to map INPUTS i.e. the "a" which threads into "f".

Enter Contravariant Functor...
NOTE:
The full name for Functor is Covariant Functor. In math, Covariance is the norm and Contravariance is the opposite.
-}

----------------------------------------

-- Contravariant Functor type class:

class Contravariant f where
  cmap :: ∀ a b. (b -> a) -> f a -> f b
  
infixl 4 cmap as >$<

-- We cannot code a Functor for C1 but we can code a Contravariant Functor:

instance contravariantC1 :: Contravariant C1 where
  cmap f (C1 g) = C1 (g <<< f)

----------------------------------------

{- 
                     OUTPUT

                    +------------+-----------+     map       +---------+
     Functor        |Int -> a    |   a -> b  +-------------->|Int -> b |
                    |            |           |               |         |
                    +------------+-----------+               +---------+
                    |            |           |     cmap      |         |
Contravariant       |b -> a      |   a -> Int+-------------->|b -> Int |
                    +------------+-----------+               +---------+

                                     INPUT
-}

----------------------------------------

{- 
Let's get some help with deciding when to implement Covariant Functor and Contravariant Functor, via Polarity:

Let’s define something called Polarity as:
- Positive Position: Any Parameter that is an OUTPUT of a Function
- Negative Position: Any Parameter that is an INPUT of a Function

foo :: a -> b
foo :: - -> +

Here it is said that foo is Covariant in b (POSITIVE) and Contravariant in a (NEGATIVE).
If the Polymorphic Parameter is in Positive Position (OUTPUT) then create a Functor instance.
If the Polymorphic Parameter is in Negative Position (INPUT) then create a Contravariant instance

EXAMPLE:

-- a is in Positive Position.
newtype F1 a = F1 (Int -> a)

instance functorF1 :: Functor F1 where
  map f (F1 g) = F1 (f <<< g)

EXAMPLE:

-- a is in Negative Position.
newtype C1 a = C1 (a -> Int)

instance contravariantC1 :: Contravariant C1 where
  cmap f (C1 g) = C1 (g <<< f)
-}

----------------------------------------

-- Invariant Functor

-- Polarity is a useful model for helping us determine if our a is in Positive or Negative Position. But what if a is in both?
newtype Both a = Both (a -> a)

{-
We cannot write a Functor Instance because a is in Negative Position and we cannot write a Contravariant Instance since a is also in Positive Position.
What we need is something that can handle both.
And that something is called an Invariant Functor.

In PureScript, the Typeclass definition is as follows: 
-}

class Invariant f where
  imap :: ∀ a b. (a -> b) -> (b -> a) -> f a -> f b

----------------------------------------

{-
Natural Transformations are simply morphisms between Functors.

Example of a Homomorphism from Maybe to Either Unit:
-}
hom :: Maybe ~> Either Unit
hom Nothing = Left unit
hom (Just x) = Right x

{-
The equivalent Type Signature as:

hom :: ∀a. Maybe a -> Either Unit a

which is equivalent to
η a : F a → G a
where
- Maybe is F
- Either Unit is G
-}

----------------------------------------

{-
If there exists an Inverse Homomorphism, then we have an Isomorphism.

Maybe and Either Unit are Isomorphic e.g.
-}

maybeToEither :: Maybe ~> Either Unit
maybeToEither Nothing = Left unit
maybeToEither (Just x) = Right x

eitherToMaybe :: Either Unit ~> Maybe
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

----------------------------------------

{-
Functor Instances for Function: Functions are also Functors.

We can think of Functions 2 ways:
- Infixed:  a -> b
- Prefixed: (->) a b

When we consider the prefixed way of writing a Function, we can see that (->) is a just a Type Constructor of 2 Parameters, a and b.
It’s no different than Either a b.

And since -> is no different than Either, we should be able to write a Functor Instance for functions:

instance functorFn :: Functor ((->) r) where
  map f g = f <<< g

((->) r) holds the Input Parameter Type fixed, i.e. we cannot map over the INPUT.
This is consistent with Functors which map OUTPUTS.
We need to apply our mapping function f to the result of g.
The code that maps f over the Output Value of g, our function by using compose.
i.e. we call our function g and then map the Output Value.
-}  

----------------------------------------

{-
Profunctor

To be able to map both INPUT and OUTPUT Types of a Function, we need to use what’s called a Profunctor.
-}

class Profunctor p where
  dimap :: ∀ a b c d. (b -> a) -> (c -> d) -> p a c -> p b d

{-
This looks a lot like Bifunctor with the notable difference of the reversal of the Types in the Type Signature of the first mapping function, viz. b -> a.
That means that a Profunctor is Contravariant in the first Parameter (maps INPUTS) and Covariant in the second (maps OUTPUTS).
-}

instance profunctorFn :: Profunctor (->) where
  dimap :: ∀ a b c d. (b -> a) -> (c -> d) -> (a -> c) -> (b -> d)
  dimap f g h = g <<< h <<< f

----------------------------------------

test :: Effect Unit
test = do
  log "test"  