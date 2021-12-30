-- Traversables
module Ch16a where

import Control.Alt (class Alt, (<|>))
import Data.Identity
import Data.Int.Bits ((.&.))
import Data.List
import Data.List (fromFoldable)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, sequence, traverse)
import Effect (Effect)
import Effect.Console (log)
import Prelude

{-
Let’s consider a common scenario that we will encounter from time to time.
First, we have an Array of Ints that we want to map over with what is called an Effectful function,
i.e. a function that does NOT produce a Pure Value, but a Value in a Context:
-}

even :: Int -> Boolean
even x = x .&. 1 == 0

half :: Int -> Maybe Int
half x = if even x then Just (x `div` 2) else Nothing

halvesA = map half [2, 4, 6] -- [Just 1, Just 2, Just 3]

halvesB = map half [2, 3, 4] -- [Just 1, Nothing, Just 2]

{-
But we don’t want an Array of Maybes. We want the Ints but ONLY if we had no failures. Sound familiar?
Previously we coded "combineList".
-}

combineList :: ∀ a f. Applicative f => List (f a) -> f (List a)
combineList Nil = pure Nil
combineList (fa : fas) = Cons <$> fa <*> combineList fas

combine = combineList <<< fromFoldable -- Composition to provide a helper for better readability

withoutTraverse1  = map half [2, 4, 6]            -- [Just 1, Just 2, Just 3]

withTraverse1     = combine $ map half [2, 4, 6]  -- Just [1, 2, 3]

withoutTraverse2  = map half [2, 3, 4]            -- [Just 1, Nothing, Just 2]

withTraverse2     = combine $ map half [2, 3, 6]  -- Nothing

{-
Our combineList is pretty convenient for this common scenario.
In fact, it’s so common, that it’s been codified and generalized in a Typeclass called Traversable.

A Traversable is a Foldable Functor that can be traversed while accumulating results and effects in an Applicative:


class (Functor t, Foldable t) <= Traversable t where
  traverse :: ∀ a b f. Applicative f => (a -> f b) -> t a -> f (t b)
  
  sequence :: ∀ a f. Applicative f => t (f a) -> f (t a)


traverse:
a -> f b  is our effectful mapping function,  e.g. half :: Int -> Maybe Int.
t a       is our Foldable data structure,     e.g. Array Int.
f (t b)   is our final result,                e.g. Maybe (Array Int). 

But sometimes, we start with a Foldable structure with Values already in a Context. That’s when sequence comes in.
sequence:
t (f a)   is our Foldable of Contextual Values, e.g. Array (Maybe Int).
f (t a)   is our final result,                  e.g. Maybe (Array Int).

sequence is just traverse without the mapping function, therefore:
sequence = traverse identity
-}

--------------------------------

{-
The Traverable instance of List built into PureScript is very complex, using foldl.
We can implement a more readable version using foldr:

instance traversableList :: Traversable List where
  traverse :: ∀ a b m. Applicative m => (a -> m b) -> List a -> m (List b)
  traverse f = foldr (\x acc -> (:) <$> f x <*> acc) (pure Nil)

  sequence :: ∀ a m. Applicative m => List (m a) -> m (List a)
  sequence = traverse identity
-}

--------------------------------

{-
Context notes.

When we have a Pure Value, our computations are Pure, i.e. there’s nothing coloring our computations.
But when we have a Contextual Value, e.g. Maybe Int, computations are going to be affected by the Context that they’re performed in.
-}
pureValue     =                      (*) 5 10 == 50      -- Pure computation
contextValue  = ((*) <$> Right 5 <*> Left 10) == Left 10 -- Short-circuited computation

{-
There is a Context that has ZERO affect on computations, it’s appropriately called Identity.
Idenity is nothing more than a simple wrapper, with no additional behavior, i.e. it doesn’t affect the computation in any way, unlike Either (above).
-}
pureComputation = ((*) <$> Identity 5 <*> Identity 10) == Identity 50 -- Pure computation

--------------------------------

{-
Alt Type Class:

There is:
class Semigroup a where
  append :: a -> a -> a

We cannot create an instance for List since List has Kind, Type -> Type, but Semigroup only has Kind, Type.
Just like Bifunctor helps Functor with the extra Type, so Alt has the extra Type missing in Semigroup:

class Functor f <= Alt f where
  alt :: f a -> f a -> f a

infixl 3 alt as <|> -- Looks a bit like <> provided by Semigroup

Here Alt’s Type Parameter f has Kind Type -> Type which means we can write an instance for List:

We already have a Semigroup for List a:

instance semigroupList :: Semigroup (List a) where
  append xs ys = foldr (:) ys xs

If xs is {x1, x2, x3}, then foldr gives us (x1 : (x2 : (x3 : ys)))

Our implementation is:

instance altList :: Alt List where
  alt = (<>)
-}
altEx1 = ((1 : 2 : 3 : Nil) <|> (4 : 5 : 6 : Nil))  == (1 : 2 : 3 : 4 : 5 : 6 : Nil)
altEx2 =         ([1, 2, 3] <|> [3, 2, 1])          == [1, 2, 3, 3, 2, 1]

altEx3 = (Nothing <|> Just 10 <|> Just 11) == Just 10

--------------------------------

{-
Plus Type Class:

If Alt is like Semigroup but for a Type of Kind Type -> Type, is there a similar Typeclass to Monoid?
The answer to that is Plus:

(The following are already defined up import Control.Plus)
-}

class Alt f <= Plus f where
  empty :: ∀ a. f a -- Here empty is the analogy of mempty in Monoid

instance plusList :: Plus List where
  empty = Nil

instance plusArray :: Plus Array where
  empty = []

instance plusMaybe :: Plus Maybe where
  empty = Nothing  

--------------------------------

test :: Effect Unit
test = do
  log $ show halvesA
  log $ show halvesB
  log "---------------------------------"
  log $ show withoutTraverse1
  log $ show withTraverse1
  log $ show withoutTraverse2
  log $ show withTraverse2
  log "---------------------------------"
  log $ show pureValue
  log $ show contextValue
  log $ show pureComputation
  log "---------------------------------"
  log $ show altEx1
  log $ show altEx2
  log $ show altEx3