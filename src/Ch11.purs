-- Coding Folds

module Ch11 where

import Data.Foldable (class Foldable, foldl, foldr, foldMap)
import Data.List (List(..), (:), singleton)
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Ord (class Ord, Ordering(..), compare)
import Data.Semigroup.Foldable (foldl1)
import Data.Semiring (class Semiring, zero)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, show, type (~>), ($), discard, negate, (>), otherwise, (+), (<>), (<<<))
  
{-
Rewrite the following using fold:

reverse :: List ~> List
reverse ol = go Nil ol where
  go rl Nil = rl
  go rl (x : xs) = go (x : rl) xs  
-}

reverse :: List ~> List
reverse = foldl (\acc n -> n : acc) Nil

max :: ∀ a. Ord a => a -> a -> a
max x y = case (compare x y) of
  LT -> y
  _ -> x

-- We can rewrite the above using ">" since this is an operator alias for "greaterThan" which calls "compare" from Ord typeclass:
max' :: ∀ a. Ord a => a -> a -> a
max' x y | x > y     = x
         | otherwise = y
-- Remembering that "otherwise" is just defined to be true.

findMaxR :: ∀ a. Ord a => a -> List a -> a
findMaxR a Nil = a
findMaxR a (x : xs) = findMaxR (max a x) xs

findMaxR' :: ∀ a. Ord a => List a -> Maybe a
findMaxR' xs = go Nothing xs where
  go :: Maybe a -> List a -> Maybe a
  go m Nil = m
  go Nothing (x : xs) = go (Just x) xs
  go (Just m) (x : xs) = go (Just (max m x)) xs

findMaxR'' :: ∀ a. Ord a => List a -> Maybe a
findMaxR'' Nil = Nothing
findMaxR'' l @ (first : _) = Just $ go first l where
  go mx Nil = mx
  go mx (x : xs) = go (max x mx) xs

-- We can rewrite the above using "fold":
findMax :: ∀ a. Ord a => List a -> Maybe a
findMax = foldl chooseMax Nothing where
  chooseMax :: Maybe a -> a -> Maybe a
  chooseMax Nothing x = Just x
  chooseMax (Just m) x = Just (max m x)

findMax' :: ∀ a. Ord a => List a -> Maybe a
findMax' Nil = Nothing
findMax' l @ (x : _) = Just $ foldl max x l

-- Instead of returning Maybe, we can return a value for a "non empty list"
findMaxNE :: ∀ a. Ord a => NonEmptyList a -> a
findMaxNE (NonEmptyList (NonEmpty x xs)) = foldl max x xs

findMaxNE' :: ∀ a. Ord a => NonEmptyList a -> a
findMaxNE' (NonEmptyList f) = foldl1 max f

-- We can code our own version of foldl1
-- Remember, we pattern match against the Data Constructor NOT the Type Constructor.
-- The Data Constructor is on the right-hand side of the equal sign in a "data" definition.
foldl1' :: ∀ f a. Foldable f => (a -> a -> a) -> NonEmpty f a -> a
foldl1' fn (NonEmpty x xs) = foldl fn x xs

-- Nicer version uses NonEmpty's version of ":" (often used in pattern matching) which is ":|"
foldl1'' :: ∀ f a. Foldable f => (a -> a -> a) -> NonEmpty f a -> a
foldl1'' fn (x :| xs) = foldl fn x xs

-------------------------------------------------

sumR :: List Int -> Int
sumR Nil = 0
sumR (x : xs) = go x xs where
  go :: Int -> List Int -> Int
  go acc Nil = acc
  go acc (x : xs) = go (acc + x) xs

-- Nicer recursion:
sumR' :: List Int -> Int
sumR' = go 0 where
  go :: Int -> List Int -> Int
  go acc Nil = acc
  go acc (x : xs) = go (acc + x) xs

-- And now using foldl
sum :: List Int -> Int
sum = foldl (+) 0 

-- And more abstract via Numbers:
sum' :: ∀ a. Semiring a => List a -> a
sum' = foldl (+) zero

-- And even more abstract via Foldable
sum'' :: ∀ a f. Foldable f => Semiring a => f a -> a
sum'' = foldl (+) zero

-------------------------------------------------

data Tree a = Leaf a | Node (Tree a) (Tree a)

{- 
                     +----+
                     |Node|
                     +-+--+
                       |
             +---------+--------+
             |                  |
             |                  |
           +-v--+             +-v--+
           |Node|             |Leaf|
           +-+--+             | 99 |
             |                +----+
             |
  +----------+-------+
  |                  |
+-v--+            +--v-+
|Leaf|            |Node|
| 5  |            +-+--+
+----+              |
                    |
            +-------+---------+
            |                 |
          +-v---+          +--v--+
          |Leaf |          |Leaf |
          | -1  |          | 14  |
          +-----+          +-----+

Let's see how our "sum" function is affected by favouring the "left":
0 + 5 + (-1) + 14 + 99 = 117

And if we favour the "right":
0 + 99 + 14 + (-1) + 5 = 117

We also need to remember that we’re writing the Foldable instance for all functions, not just sum.
So, we need to consider the best way to traverse it so it works well for all Foldable operations,
e.g. Subtraction - with Subtraction, the results will NOT be the same.
-}

{- instance foldableTree :: Foldable Tree where
  foldr :: ∀ a b. (a -> b -> b) -> b -> Tree a -> b
  foldr fn b (Leaf a) = fn a b
  foldr fn b (Node (Tree a1) (Tree a2)) = fn a2 (fn a1 b)

  foldl :: ∀ a b. (b -> a -> b) -> b -> Tree a -> b
  foldl fn b (Leaf a) = fn b a
  foldl fn b (Node (Tree a1) (Tree a2)) = fn (fn b a1) a2

  foldMap :: ∀ a m. Monoid m => (a -> m) -> Tree a -> m
  foldMap fn (Leaf a) = fn a
  foldMap fn (Node (Tree a1) (Tree a2)) = fn a1
-}

toList :: ∀ a. Tree a -> List a
toList (Leaf a) = singleton a
toList (Node l r) = toList l <> toList r

{-
My poor attempt:

instance foldableTree :: Foldable Tree where
  foldr :: ∀ a b. (a -> b -> b) -> b -> Tree a -> b
  foldr fn b (Leaf a) = fn a b
  foldr fn b (Node l r) = go b (toList l <> toList r) where
    go :: b -> List a -> b
    go b Nil = b
    go b (x : xs) = go (fn x b) xs

  foldl :: ∀ a b. (b -> a -> b) -> b -> Tree a -> b
  foldl fn b (Leaf a) = fn b a
  foldl fn b (Node l r) = go b (toList l <> toList r) where
    go :: b -> List a -> b
    go b Nil = b
    go b (x : xs) = go (fn b x) xs

  foldMap :: ∀ a m. Monoid m => (a -> m) -> Tree a -> m
  foldMap fn (Leaf a) = fn a
  foldMap fn (Node l r) = ???
-}

instance foldableTree :: Foldable Tree where
  foldr fn acc = foldr fn acc <<< toList
  foldl fn acc = foldl fn acc <<< toList
  foldMap fn = foldMap fn <<< toList

-------------------------------------------------

test :: Effect Unit
test = do
  log $ show $ reverse (10 : 20 : 30 : Nil)
  log $ show $ max (-1) 99
  log $ show $ max "aa" "z"
  log $ show $ findMaxR 0 (37 : 311 : -1 : 2 : 84 : Nil)
  log $ show $ findMaxR "" ("a" : "bbb" : "c" : Nil)
  log $ show $ findMaxR' (37 : 311 : -1 : 2 : 84 : Nil)
  log $ show $ findMaxR' ("a" : "bbb" : "c" : Nil)

  log $ show $ findMax (37 : 311 : -1 : 2 : 84 : Nil)
  log $ show $ findMax ("a" : "bbb" : "c" : Nil)

  log $ show $ findMaxNE (NonEmptyList $ 37 :| (311 : -1 : 2 : 84 : Nil))
  log $ show $ findMaxNE (NonEmptyList $ "a" :| ("bbb" : "c" : Nil))

  log $ show $ findMaxNE' (NonEmptyList $ 37 :| (311 : -1 : 2 : 84 : Nil))
  log $ show $ findMaxNE' (NonEmptyList $ "a" :| ("bbb" : "c" : Nil))

  log $ show $ sum (1 : 2 : 3 : Nil)
  log $ show $ sum' (1.0 : 2.0 : 3.0 : Nil)
  log $ show $ sum'' [1, 2, 3]
  log $ show $ sum'' [1.0, 2.0, 3.0]

  log $ show $ toList
    (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))

  log $ show $ sum''
    (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))  


{- 
NOTE - We used Semiring typeclass which is defined as:

class Semiring a where
  add :: a -> a -> a
  zero :: a
  mul :: a -> a -> a
  one :: a

infixl 6 add as +

infixl 7 mul as *
-}  