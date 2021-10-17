module Ch5 where

import Prelude (Unit, (+), show, discard)
import Data.List (List(..), (:)) -- List(..) is shorthand for List(Nil, Cons) i.e. all data constructors
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)

flip :: ∀ a b c. (a -> b -> c) -> b -> a -> c
flip f b a = f a b

-- If we included parentheses, our eyes would trick us into writing another variation
flip2 :: ∀ a b c. (a -> b -> c) -> (b -> a -> c)
flip2 f = \ x y -> f y x

const :: ∀ a b. a -> b -> a
const x _ = x

apply :: ∀ a b. (a -> b) -> a -> b
apply f x = f x

infixr 0 apply as $

applyFlipped :: ∀ a b. a -> (a -> b) -> b
applyFlipped = flip apply

infixl 1 applyFlipped as #

singleton :: ∀ a. a -> List a
singleton x = x : Nil

null :: ∀ a. List a -> Boolean
null Nil = true
null _ = false

snoc :: ∀ a. List a -> a -> List a
snoc Nil x = singleton x
snoc (y : ys) x = y : snoc ys x

lengthNotTailRecursive :: ∀ a. List a -> Int
lengthNotTailRecursive Nil = 0
lengthNotTailRecursive (_ : xs) = 1 + lengthNotTailRecursive xs

length :: ∀ a. List a -> Int
length = go 0 where
  go :: Int -> List a -> Int
  go acc Nil = acc
  go acc (_ : xs) = go (acc + 1) xs

head :: ∀ a. List a -> Maybe a
head Nil = Nothing
head (x : _) = Just x

tail :: ∀ a. List a -> Maybe (List a)
tail Nil = Nothing
tail (_ : xs) = Just xs

last :: ∀ a. List a -> Maybe a
last Nil = Nothing
last (x : Nil) = Just x
last (_ : xs) = last xs

-- -----------------------------------------------

test :: Effect Unit
test = do
  log (show (flip const 1 2))

  log $ show $ flip const 1 2

  flip const 1 2 # show # log

  log $ show $ singleton "xyz"

  log $ show $ null Nil

  log $ show $ null ("abc" : Nil)

  log $ show $ snoc (1 : 2 : Nil) 3

  log $ show $ lengthNotTailRecursive $ 1 : 2 : 3 : Nil

  log $ show $ length $ 1 : 2 : 3 : Nil

  log $ show (head Nil :: Maybe Unit)

  log $ show $ head ("abc" : "123" : Nil)

  log $ show $ tail (Nil :: List Unit) -- OR log $ show (tail Nil :: Maybe (List Unit))

  log $ show $ tail ("abc" : "123" : Nil)

  log $ show $ last (Nil :: List Unit)

  log $ show $ last $ "a" : "b" : "c" : Nil