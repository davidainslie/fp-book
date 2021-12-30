-- Applicative Functors, Traversables and Alternatives
module Ch16 where

import Data.List
import Data.List (fromFoldable)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude

{-
Function in a context e.g.
Just (_ * 10) :: Maybe (Int -> Int)

We could apply with 20 as:
-}
functionInContext :: Maybe Int
functionInContext = (\f -> f 20) <$> Just (_ * 10) :: Maybe Int -- f is effectively set to (_ * 10)

{-
But what happens if we have Just 20 instead of 20?
We will end up with horrible nesting e.g.
-}
horribleFunctionInContext :: Maybe (Maybe Int)
horribleFunctionInContext = (\f -> f <$> Just 20) <$> Just (_ * 10) :: Maybe (Maybe Int)

{-
To avoid the nesting we could write a helper for Maybe:
-}
applyToMaybe :: ∀ a b. Maybe (a -> b) -> Maybe a -> Maybe b
applyToMaybe Nothing _ = Nothing
applyToMaybe (Just f) m = f <$> m

{-
However, we would need a helper for every "context".
Another type class to the rescue, to abstract out the behaviour:

class Functor f <= Apply f where
  apply :: ∀ a b. f (a -> b) -> f a -> f b
  
infixl 4 apply as <*>
-}

functionInContextWithoutNesting :: Maybe Int
functionInContextWithoutNesting = Just (_ * 10) <*> Just 20 :: Maybe Int

--------------------------------

{-
Let's try to understand this confusing one:
-}
data Tuple a b = Tuple a b

instance showTuple :: (Show a, Show b) => Show (Tuple a b) where
  show (Tuple a b) = "(Tuple " <> show a <> " " <> show b <> ")"  

x :: ∀ a b. Maybe a -> Maybe b -> Maybe (Tuple a b)
x ma mb = Tuple <$> ma <*> mb
{-
ma :: Maybe a
Tuple <$> ma :: Maybe (b -> Tuple a b)
mb :: Maybe b
Tuple <$> ma <*> mb :: Maybe (Tuple a b)

This is a common pattern: first "map" then "apply"
-}

{-
Another example:
-}
data Threeple a b c = Threeple a b c

threeple :: ∀ a b c. Maybe a -> Maybe b -> Maybe c -> Maybe (Threeple a b c)
threeple mx my mz = Threeple <$> mx <*> my <*> mz
{-
As soon as we have our function in a Context, Threeple (which is a Data Constructor) we can start using <*>.

And if Threeple is already within the Context when then immediately apply without needing to map e.g.
-}
threeple' :: ∀ a b c. Maybe a -> Maybe b -> Maybe c -> Maybe (Threeple a b c)
threeple' mx my mz = Just Threeple <*> mx <*> my <*> mz

{-
What if we try to generalise this?

threeple :: ∀ a b c f. f a -> f b -> f c -> f (Threeple a b c)
threeple mx my mz = ??? Threeple <*> mx <*> my <*> mz

??? needs to be a Data Constructor.

And once again, the way to do this without knowing the Type up front is to use a Typeclass.
The Typeclass in question that has the Data Constructor behaviour is Applicative:

class Apply f <= Applicative f where
  pure :: ∀ a. a -> f a

we end up with the generic version:  
-}
threepleG :: ∀ a b c f. Applicative f => f a -> f b -> f c -> f (Threeple a b c)
threepleG mx my mz = pure Threeple <*> mx <*> my <*> mz

--------------------------------

{-
instance functorMaybe :: Functor Maybe where
  map f (Just a) = Just (f a)
  map _ Nothing = Nothing

instance applyMaybe :: Apply Maybe where
  apply (Just f) m = f <$> m
  apply Nothing _ = Nothing

instance applicativeMaybe :: Applicative Maybe where
  pure = Just
-}

--------------------------------

{-
Applicative for Product

Let's start with Tuple:
-}
instance functorTuple :: Functor (Tuple a) where
  map :: ∀ b c. (b -> c) -> Tuple a b -> Tuple a c
  map fn (Tuple a b) = Tuple a $ fn b

instance applyTuple :: Semigroup a => Apply (Tuple a) where
  apply :: ∀ b c. Tuple a (b -> c) -> Tuple a b -> Tuple a c
  apply (Tuple a1 fn) (Tuple a2 b) = Tuple (a1 <> a2) (fn b)

instance applicativeTuple :: Monoid a => Applicative (Tuple a) where
  pure :: ∀ b. b -> Tuple a b
  pure b = Tuple mempty b

{- And Threeple... -}

instance functorThreeple :: Functor (Threeple a b) where
  map :: ∀ c d. (c -> d) -> Threeple a b c -> Threeple a b d
  map fn (Threeple a b c) = Threeple a b $ fn c

instance applyThreeple :: (Monoid a, Monoid b) => Apply (Threeple a b) where
  apply :: ∀ c d. Threeple a b (c -> d) -> Threeple a b c -> Threeple a b d
  apply (Threeple a1 b1 fn) (Threeple a2 b2 c) = Threeple (a1 <> a2) (b1 <> b2) (fn c)

instance applicativeThreeple :: (Monoid a, Monoid b) => Applicative (Threeple a b) where
  pure :: ∀ c. c -> Threeple a b c
  pure c = Threeple mempty mempty c

--------------------------------

{-
Applicative Instance for Sum Types

We've done Maybe above, so here is a custom example:
-}

data Things a b c
  = Thing1 a
  | Thing2 b c -- PRODUCT

instance functorThings :: Functor (Things a b) where
  map _ (Thing1 a) = Thing1 a
  map fn (Thing2 b c) = Thing2 b $ fn c

instance applyThings :: Monoid b => Apply (Things a b) where
  apply (Thing2 b1 fn) (Thing2 b2 c) = Thing2 (b1 <> b2) (fn c)
  apply _ (Thing1 a) = Thing1 a
  apply (Thing1 fn) _ = Thing1 fn

instance applicativeThings :: Monoid b => Applicative (Things a b) where
  pure c = Thing2 mempty c

--------------------------------

{-
Combine effects e.g.
-}
combineList :: ∀ a f. Applicative f => List (f a) -> f (List a)
combineList Nil = pure Nil
combineList (fa : fas) = Cons <$> fa <*> combineList fas

{-
In the case where f is Maybe, we’ll get
  a Maybe (List a)

And if f is Either a then we’ll get an
  Either a (List b)
-}

{-
combineList recursive explanation:

Cons :: ∀ a. a -> List a -> List a

fa :: f a
Cons <$> fa :: f (List a -> List a)
OR
map Cons fa =
map (a -> List a -> List a) fa =
f (List a -> List a)

Now we have a function inside a Context, so we expect to next use "apply".

combineList fas :: f (List a)

So
Cons <$> fa <*> combineList fas
becomes
Cons <$> fa <*> f (List a) =
apply f (List a -> List a) f (List a) =
f (List a)
-}

-- Example with Maybe:
combinedMaybes :: Maybe (List Int)
combinedMaybes = combineList (Just 1 : Just 2 : Just 3 : Nil) -- (Just (1 : 2 : 3 : Nil))

combinedJust :: Maybe (List Int)
combinedJust = combineList (fromFoldable [Just 1, Just 2, Just 3]) -- (Just (1 : 2 : 3 : Nil))

combinedJustNothing :: Maybe (List Int)
combinedJustNothing = combineList (fromFoldable [Just 1, Nothing, Just 3]) -- Nothing

{-
Given:
  combineList (fromFoldable [Just 1, Just 2])

Apply fromFoldable:
  combineList (Just 1 : Just 2 : Nil)

Substitute:
  Cons <$> Just 1 <*> combineList (Just 2 : Nil)

Do map i.e. <$>:
  Just (Cons 1) <*> combineList (Just 2 : Nil)

Substitute:
  Just (Cons 1) <*> (Cons <$> Just 2 <*> combineList Nil)

Do map i.e. <$>:
  Just (Cons 1) <*> (Just (Cons 2) <*> combineList Nil)

Substitute:
  Just (Cons 1) <*> (Just (Cons 2) <*> pure Nil)

Do apply i.e. <*>:
  Just (Cons 1) <*> (Just (Cons 2 Nil))

Do apply i.e. <*>:
  Just (Cons 1 (Cons 2 Nil))

Rewrite:
  Just (1 : 2 : Nil)               
-}

-- Example with Either:
combinedRight :: Either String (List Int)
combinedRight = combineList (fromFoldable [Right 1, Right 2, Right 3]) -- (Right (1 : 2 : 3 : Nil))

combinedLeftRight :: Either String (List Int)
combinedLeftRight = combineList (fromFoldable [Right 1, Left "err", Right 3, Left "err2"]) -- (Left "err")

--------------------------------

-- Example:
tuples :: Array (Tuple Int Int)
tuples = Tuple <$> [10,20] <*> [3, 4] -- [(Tuple 10 3), (Tuple 10 4), (Tuple 20 3), (Tuple 20 4)]

--------------------------------

{-
An Applicative is Commutative IFF (if and only iff):

f <$> x <*> y = flip f <$> y <*> x

This says that if we apply Parameters x and y to f and it is equal to flipping the Parameters to the function and then applying x and y in reverse,
then <*> is Commutative.
-}

-- Works for Maybe:
maybe     = (-) <$> Just 3 <*> Just 9         -- Just -6
maybeFlip = flip (-) <$> Just 9 <*> Just 3    -- Just -6

-- DOES NOT work for Either:
either      = (+) <$> Left "err1" <*> Left "err2"       -- Left "err1"
eitherFlip  = flip (+) <$> Left "err2" <*> Left "err1"  -- Left "err2"

-- DOES NOT work for Array (or List):
array     = (+) <$> [2, 3] <*> [7, 13]       -- [9, 15, 10, 16]
arrayFlip = flip (+) <$> [7, 13] <*> [2, 3]  -- [9, 10, 15, 16]

--------------------------------

test :: Effect Unit
test = do
  log $ show $ functionInContext
  log $ show $ horribleFunctionInContext
  log $ show $ functionInContextWithoutNesting
  log "------------------------------------------"
  log $ show $ Tuple "abc" (_ * 10) <*> Tuple "123" 42 -- Tuple "abc123" 420
  log $ show $ pure (_ * 10) <*> Tuple "abc" 42 -- Tuple "abc" 420
  log $ show $ Tuple "abc" (_ * 10) <*> pure 42 -- Tuple "abc" 420
  log "------------------------------------------"
  log $ show $ combinedJust
  log $ show $ combinedJustNothing
  log "------------------------------------------"
  log $ show $ combinedRight
  log $ show $ combinedLeftRight
  log "------------------------------------------"
  log $ show $ tuples