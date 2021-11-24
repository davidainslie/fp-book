-- Functors

module Ch12 where

import Data.Either (Either(..))
import Data.EuclideanRing (class EuclideanRing)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude

-- Say we want to calculate:
-- f(x) = (83 / x) * 10

-- Without Functor:
divide :: ∀ a. Eq a => EuclideanRing a => a -> a -> Maybe a
divide x y
  | y == zero = Nothing
  | otherwise = Just $ x / y

-- and so computing the above formula without Functor:
f :: Number -> Maybe Number
f x = case divide 83.0 x of
  Nothing     -> Nothing
  Just result -> Just $ result * 10.0

--------------------------------------------   

-- We could to the same above but instead use Either:
divide' :: ∀ b. Eq b => EuclideanRing b => b -> b -> Either String b
divide' x y
  | y == zero = Left "Divide by zero"
  | otherwise = Right $ x / y

f' :: Number -> Either String Number
f' x = case divide' 83.0 x of
  Left reason   -> Left reason
  Right result  -> Right $ result * 10.0  

--------------------------------------------   

test :: Effect Unit
test = do
  log $ show $ f 5.0
  log $ show $ f 0.0