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

{-
When we work with Simple Types i.e. not Higher-kinded, we can write functions like:

f :: ∀ a b. a -> b

g :: ∀ b c. b -> c

h :: ∀ c d. c -> d

and then compose them:
hdf :: ∀ a d. a -> d
hfg = h <<< g <<< f

or function application:
fgh :: ∀ a d. a -> d
fgh x = h $ g $ f x
-}

{- 
What if one of our functions above allows for failure?
g :: ∀ b c. b -> Maybe c -- Now returns a Higher-kinded Type

We can still compose with say f:
gf :: ∀ a c. a -> Maybe c
gf = g <<< f

and function application:
fg :: ∀ a c. a -> Maybe c
fg x = g $ f x

BUT we are stuck when we continue Compose or Apply with h.
h takes a c and not Maybe c.
-}

--------------------------------------------   

{-
Here’s the definition of a Functor in PureScript:

class Functor f where
  map :: ∀ a b. (a -> b) -> f a -> f b

infixl 4 map as <$>

The map Method allows us to Apply the function a -> b to the a within the Context of f and modify it WITHOUT changing the Context.

Functor instance for Maybe:
 
instance functorMaybe :: Functor Maybe where
  map _ Nothing  = Nothing
  map f (Just x) = Just $ f x

and with parentheses we get another perspective:

class Functor f where
  map :: ∀ a b. (a -> b) -> (f a -> f b)

We can say that map lifts the function into the Functor, i.e. we take a function that operates on Simple Types (also referred to as Pure Values) and lifts it to work on Functor Types.  
-}

--------------------------------------------   

test :: Effect Unit
test = do
  log $ show $ f 5.0
  log $ show $ f 0.0