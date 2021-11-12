-- Fold is generalised way to iterate with State.
-- Unlike a recursive function a Fold cannot "short circuit".

-- Folds are called Catamorphisms because they break down the original structure.
-- And there's "unfold" which are called Anamorphisms since they build up structure from a single start value.

{- 
The Foldable typeclass:

class Foldable f where
  foldr :: ∀ a b. (a -> b -> b) -> b -> f a -> b
  
  foldl :: ∀ a b. (b -> a -> b) -> b -> f a -> b
  
  foldMap :: ∀ a m. Monoid m => (a -> m) -> f a -> m
-}

{- 
foldMap may need some explanation:

(a -> m)  -- function to convert the element to a Monoid
f a       -- Foldable type `f` e.g. List of Array

foldMap works with Monoids and can use the Type's mempty as its Initial Value.

The variable f is being used here as the collection or structure that we're folding over.
It stands for Functor since most Functors are also Foldable.
-}

{-
Examples:

foldr (-) 99 [1, 2, 3] == 1 - (2 - (3 - 99)) == -97

in verbose terms would be:
foldr (\n acc -> n - acc) 99 [1, 2, 3] == -97

foldl (-) 99 [1, 2, 3] == ((99 - 1) - 2) - 3 == 93

in verbose terms would be:
foldl (\acc n -> acc - n) 99 [1, 2, 3] == 93
-}

{- 
NOTE on Kind:

class Eq a where
  eq :: a -> a -> Boolean

The Type parameter to the Eq class is a and its Kind is Type.

Types categorise Values in the same way as Kinds categorise Types i.e. a Value has a Type, and a Type has a Kind.
The type of a Value is called Type.
The type of a Type is called Kind.

Looking at the Foldable typeclass we see that the Type parameter is f,
and when we look at how f is used in the Type signatures of the methods, we see that f takes another Type.

That means that f's Kind is Type -> Type,
i.e. f is a Type constructor.

Just like Data constructors, we can think of Type constructors like functions, except their parameters are Types.

Examples of Type signatures for Data constructors:

Just :: ∀ a. a -> Maybe a

Left :: ∀ a. a -> Either a b

Right :: ∀ b. b -> Either a b


Examples of Kind signatures for Type constructors:

Maybe :: Type -> Type

Either :: Type -> Type -> Type


so an instance of Foldable for List would be:

instance foldableList :: Foldable List where -- List's Kind is Type -> Type matching the f
  foldr :: ∀ a b. (a -> b -> b) -> b -> List a -> b
  ...

whereas, fo Either (which is Type -> Type -> Type i.e. not matching f) we would have to fix the first parameter and replace f with Either a.
-}

module Ch10 where

import Effect (Effect)
import Effect.Console (log)
import Prelude

test :: Effect Unit
test = do
  log "blah"