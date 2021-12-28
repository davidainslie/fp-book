-- Coding Functors

module Ch15 where

import Data.Functor.Contravariant (class Contravariant, cmap, (>$<))
import Data.Foldable (class Foldable, foldl)
import Data.Int.Bits ((.&.))
import Data.List (List(..), (:))
import Data.Profunctor (class Profunctor, dimap)
import Data.Semiring (class Semiring)
import Data.String (length)
import Effect (Effect)
import Effect.Console (log)
import Prelude

---------------------------------------

even :: Int -> Boolean
even x = x .&. 1 == 0

odd :: Int -> Boolean
odd = not $ even

odd' :: Int -> Boolean
odd' x = x .&. 1 == 1

odd'' :: Int -> Boolean
odd'' x = x `mod` 2 == 1

---------------------------------------

{-
If all our predicates are essentially 1 parameter functions we could generalise, even for predicates with 2 parameters because of the power of "uncurry" (and "curry").

member :: ∀ k v. Ord k => k -> Map k v -> Boolean
uncurry to:
member :: ∀ k v. Ord k => Tuple k (Map k v) -> Boolean
-}

data Predicate a = Predicate (a -> Boolean)

{-
The Polymorphic Parameter, a, is in Negative Position, meaning that we cannot write a Functor Instance, but instead can write a Contravariant Instance.

Let’s first write a helper function called runPredicate that will run the embedded function for us.
You’ll need to extract the function from Predicate then call it.
-}

runPredicate :: ∀ a. Predicate a -> a -> Boolean    -- Predicate a..... left hand side of type declaration
runPredicate (Predicate f) x = f x                  -- Predicate f..... right hand side of type declaation i.e. destructure

{-
Remember:

class Contravariant f where
  cmap :: ∀ a b. (b -> a) -> f a -> f b
-}

instance contravariantPredicate :: Contravariant Predicate where
  cmap f (Predicate g) = Predicate (g <<< f) -- We call "f" first to map the INPUT to something "g" expects.

---------------------------------------
{-
Model Folds in a Type.

The first step to modeling anything is to make sure we fully understand the thing we’re trying to model.
So let’s list out the attributes of a Fold-Left Operation (which includes an extra "extract" function, to extract the final result we’re interested in):

s0 :: s               -- initial State
x :: a                -- input value
step :: s -> a -> s   -- function called at each step
extract :: s -> r     -- function to extract result from Final State

Realize that the "extract" function can always be "identity" when we want the Full State.
We don’t always want the full State but maybe a portion of it,
e.g. we may have a Tuple in the State but we only want the fst of that Tuple since snd was just a working value.

It turns out that this model is very close to something called a Moore Machine.
Moore Machine is a definition for a Finite State Machine.

Let’s look at the Type Signature for foldl to see how Moore Machines relate:

foldl :: ∀ a s f. Foldable f => (s -> a -> s) -> s -> f a -> s

- The first Parameter to foldl is the step function in our model.
- The second Parameter to foldl is the initial State.
- foldl’s third Parameter is a Foldable, which contains the input values
-}

data Moore s a b = Moore s (s -> b) (s -> a -> s)
-- Here s is the State Type, a is the Input Type and b is the Output Type.

{-
Remember:
class Profunctor p where
  dimap :: ∀ a b c d. (b -> a) -> (c -> d) -> p a c -> p b d
-}

{-
Type Parameters:
Moore s a b -- before mapping
Moore s c d -- after mapping

Data Constructor Parameters:
Moore s (s -> b) (s -> a -> s) -- before mapping
Moore s (s -> d) (s -> c -> s) -- after mapping
-}
instance profunctorMoore :: Profunctor (Moore s) where
  -- Remember, when we write the specialized Type Signature for dimap, we are ONLY concerned with the Type Constructor Parameters.
  -- We’re going from a (in Moore s a b) to c (in Moore s b d), yet f is c -> a. It’s backwards, which means that f is Contravariant.
  -- We’re going from b (in Moore s a b) to d (in Moore s b d) and g is b -> d, which matches meaning that g is Covariant.
  dimap :: ∀ a b c d. (c -> a) -> (b -> d) -> Moore s a b -> Moore s c d

  -- When we are coding the dimap function, we are now ONLY concerned with the Data Constructor Parameters.
  -- s0 is the Initial State, output is the Output Function and transition is the Transition Function.:
  dimap f g (Moore s0 output transition) =
    Moore s0 (g <<< output) (\s -> transition s <<< f)

{-
Explaining the last function:

transition                  :: s -> a -> s          -- Moore s a b
s                           :: s
transition s                :: a -> s
f                           :: c -> a
transition s <<< f          :: c -> s
\s -> transition s <<< f    :: s -> c -> s          -- Moore s c d
-}

-- sumR :: List Int -> Int

--      Moore s   a   b
addr' :: Moore Int Int Int
addr' = Moore 0 identity (+)
{-
Here the first Parameter to Moore, our State, is an Int since it’s the Accumulator of our sum.
The second Parameter is an Int, i.e. each element of our Foldable is an Int.
The third Parameter is also an Int, i.e. after adding up all the Ints we expect an Int as our result.

Realize that this function just produces a Moore, which is just a definition or instructions on how to Fold Ints.
We need an Interpreter of this Value to actually perform the operation.

We "primed" the above function because that is not the implementation we want - It's just a first attempt.
It would be nice if addr also worked for Numbers. In fact, it would be nice if it worked for anything that supports (+).
-}

addr'' :: ∀ a. Semiring a => Moore a a a
addr'' = Moore zero identity add

-- But since "add" is a type alias for (+) we can stick with that:
addr :: ∀ a. Semiring a => Moore a a a
addr = Moore zero identity (+)

runFoldL :: ∀ s a b f. Foldable f => Moore s a b -> f a -> b
runFoldL (Moore s0 output transition) xs = output $ foldl transition s0 xs

-- OR Eta reduced:
runFoldL''' :: ∀ s a b f. Foldable f => Moore s a b -> f a -> b
runFoldL''' (Moore s0 output transition) = output <<< foldl transition s0

sizer :: Moore Int String String
sizer = dimap length (\n -> "Size is " <> show n) addr

---------------------------------------

test :: Effect Unit
test = do
  log $ show $ odd 0
  log $ show $ odd 1
  log $ show $ odd' 0
  log $ show $ odd' 1
  log $ show $ odd'' 0
  log $ show $ odd'' 1
  log "------------------------------------"
  log $ show $ runPredicate (Predicate odd) $ 10
  log $ show $ runPredicate (Predicate odd) $ 11
  log "------------------------------------"
  -- These tests "cmap" the function over the Predicate to augment the INPUT value BEFORE the Predicate function is executed.
  log $ show $ runPredicate (cmap (_ + 1) (Predicate odd)) 10
  log $ show $ runPredicate (cmap (_ + 2) (Predicate odd)) 10
  log $ show $ runPredicate ((_ + 1) >$< (Predicate odd)) 10
  log $ show $ runPredicate ((_ + 2) >$< (Predicate odd)) 10
  log "------------------------------------"
  log $ show $ runFoldL addr [1, 2, 3]
  log $ show $ runFoldL addr (1.0 : 2.0 : 3.0 : Nil)
  log "------------------------------------"
  log $ show $ runFoldL sizer [ "This", "is", "the", "test" ]