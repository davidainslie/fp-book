-- Coding Folds

module Ch11FoldableTree where

import Data.Foldable (class Foldable, foldl, foldr, foldMap)
import Data.List (List, singleton)
import Data.Semiring (class Semiring, zero)
import Effect (Effect)
import Effect.Console (log)
import Prelude (discard, ($), Unit, (<<<), (<>), show, negate, (+))

sum :: ∀ a f. Foldable f => Semiring a => f a -> a
sum = foldl (+) zero

{- 
The Foldable typeclass:

class Foldable f where
  foldr :: ∀ a b. (a -> b -> b) -> b -> f a -> b
  
  foldl :: ∀ a b. (b -> a -> b) -> b -> f a -> b
  
  foldMap :: ∀ a m. Monoid m => (a -> m) -> f a -> m
-}

data Tree a = Leaf a | Node (Tree a) (Tree a)

class ToList f where
  toList :: ∀ a. f a -> List a

newtype RightFirstTree a = RightFirstTree (Tree a)

newtype LeftFirstTree a = LeftFirstTree (Tree a)

instance toListRightFirstTree :: ToList RightFirstTree where
  toList (RightFirstTree (Leaf x)) = singleton x
  toList (RightFirstTree (Node lt rt)) =
    toList (RightFirstTree rt) <> toList (RightFirstTree lt)

instance toListLeftFirstTree :: ToList LeftFirstTree where
  toList (LeftFirstTree (Leaf x)) = singleton x
  toList (LeftFirstTree (Node lt rt)) =
    toList (LeftFirstTree lt) <> toList (LeftFirstTree rt)

instance foldableRightFirstTree :: Foldable RightFirstTree where
  foldr f acc = foldr f acc <<< toList

  foldl f acc = foldl f acc <<< toList

  foldMap f = foldMap f <<< toList


instance foldableLeftFirstTree :: Foldable LeftFirstTree where
  foldr f acc = foldr f acc <<< toList

  foldl f acc = foldl f acc <<< toList

  foldMap f = foldMap f <<< toList

-------------------------------------------------

test :: Effect Unit
test = do
  -- Prints (5 : -1 : 14 : 99 : Nil)
  log $ show $ toList $ LeftFirstTree
    (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))

  -- Prints 117
  log $ show $ sum $ LeftFirstTree
    (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))

  -- Prints (99 : 14 : -1 : 5 : Nil)
  log $ show $ toList $ RightFirstTree
    (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))

  -- Prints 117
  log $ show $ sum $ RightFirstTree
    (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))     