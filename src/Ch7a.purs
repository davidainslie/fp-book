-- Coding Typeclasses

module Ch7a where

import Prelude (Unit, discard, (==), ($), (<), (>), (<=), {- (>=) -} (||), (<>))
import Data.Eq (class Eq)
import Data.Ord (class Ord, Ordering(..), compare)
import Data.Show (class Show, show)
import Effect (Effect)
import Effect.Console (log)

data Maybe a = Nothing | Just a

instance eqMaybe :: Eq a => Eq (Maybe a) where
  eq :: Maybe a -> Maybe a -> Boolean
  eq (Just a1) (Just a2) = a1 == a2
  eq Nothing Nothing = true
  eq _ _ = false

instance ordMaybe :: (Eq a, Ord a) => Ord (Maybe a) where
  compare :: Maybe a -> Maybe a -> Ordering
  compare (Just a1) (Just a2) = compare a1 a2
  compare Nothing Nothing = EQ
  compare _ Nothing = GT
  compare Nothing _ = LT

greaterThanOrEq :: ∀ a. Ord a => a -> a -> Boolean
greaterThanOrEq a1 a2 = comparison == GT || comparison == EQ where
  comparison :: Ordering
  comparison = compare a1 a2

-- Not we commented out the above Prelude import for >= so we can (re-)define here
infixl 4 greaterThanOrEq as >= 

instance showMaybe :: Show a => Show (Maybe a) where
  show :: Maybe a -> String
  show (Just x) = "Just" <> " " <> (show x)
  show Nothing = "Nothing"

-- -----------------------------------------------

test :: Effect Unit
test = do
  log $ show $ Just 5 == Just 5
  log $ show $ Just 5 == Just 2
  log $ show $ Just 5 == Nothing
  log $ show $ Nothing == Just 5
  log $ show $ Nothing == (Nothing :: Maybe Unit)
  log "--------------------------"
  log $ show $ Just 1 < Just 5
  log $ show $ Just 5 <= Just 5
  log $ show $ Just 5 > Just 10
  log $ show $ Just 10 >= Just 10
  log $ show $ Just 99 > Nothing
  log $ show $ Just 99 < Nothing
  log "--------------------------"
  log $ show $ Just "abc"  
  log $ show $ (Nothing :: Maybe Unit)  