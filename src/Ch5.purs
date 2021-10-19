module Ch5 where

import Prelude (Unit, (+), (-), (==), (<), (>=), (/=), negate, show, discard, type (~>))
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

init :: ∀ a. List a -> Maybe (List a)
init Nil = Nothing
init xs = Just $ go xs where
  go :: List a -> List a
  go Nil = Nil
  go (_ : Nil) =  Nil
  go (x : xs) = x : go xs

uncons :: ∀ a. List a -> Maybe { head :: a, tail :: List a }
uncons Nil = Nothing
uncons (x : xs) = Just { head: x,  tail: xs }

index :: ∀ a. List a -> Int -> Maybe a
index Nil _ = Nothing
index _ i | i < 0 = Nothing
index (x : _) 0 = Just x
index (_ : xs) i = index xs (i - 1)

infixl 8 index as !!

findIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findIndex p xs = go 0 xs where
  go :: Int -> List a -> Maybe Int
  go _ Nil = Nothing
  go i (x : xs) | p x = Just i
  go i (_ : xs) = go (i + 1) xs

findLastIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex p xs = go 0 xs Nothing where
  go :: Int -> List a -> Maybe Int -> Maybe Int
  go _ Nil last = last
  go i (x : xs) last | p x = go (i + 1) xs (Just i)
  go i (_ : xs) last = go (i + 1) xs last

-- Equivalent to:
-- reverse :: ∀ a. List a -> List a
-- ~> is a Natural Transformation which is a binary operator on Types NOT Values (so we only see it in the type signature).
-- The Type on the left and right are both Functors
reverse :: List ~> List 
reverse xs = go Nil xs where
  go :: ∀ a. List a -> List a -> List a
  go acc Nil = acc
  go acc (x : xs) = go (x : acc) xs

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

  log $ show $ init (Nil :: List Unit)

  log $ show $ init (1 : Nil)

  log $ show $ init (1 : 2 : Nil)

  log $ show $ init (1 : 2 : 3 : Nil)

  log $ show $ uncons (Nil :: List Unit)
  
  log $ show $ uncons (1 : Nil)
  
  log $ show $ uncons (1 : 2 : 3 : Nil)

  log $ show $ index (1 : Nil) 4
  
  log $ show $ index (1 : 2 : 3 : Nil) 1
  
  log $ show $ flip index 1 (1 : 2 : 3 : Nil)
  
  log $ show $ index (Nil :: List Unit) 0

  log $ show $ index (1 : 2 : 3 : Nil) (-99)

  log $ show $ (1 : 2 : 3 : Nil) !! 1

  log $ show $ findIndex (_ >= 2) (1 : 2 : 3 : Nil)

  log $ show $ findIndex (_ >= 99) (1 : 2 : 3 : Nil)

  log $ show $ findIndex (10 /= _) (Nil :: List Int)

  log $ show $ findLastIndex (_ == 10) (Nil :: List Int)

  log $ show $ findLastIndex (_ == 10) (10 : 5 : 10 : -1 : 2 : 10 : Nil)

  log $ show $ findLastIndex (_ == 10) (11 : 12 : Nil)

  log $ show $ reverse (10 : 20 : 30 : Nil)