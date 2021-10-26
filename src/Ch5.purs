-- Functional foundations

module Ch5 where

import Prelude (Unit, (+), (-), (==), (<), (>), (>=), (<=), (/=), (>>>), (<<<), max, negate, otherwise, show, discard, type (~>))
import Data.List (List(..), (:)) -- List(..) is shorthand for List(Nil, Cons) i.e. all data constructors
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
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
reverse = go Nil where -- Note we are point free
  go :: ∀ a. List a -> List a -> List a
  go acc Nil = acc
  go acc (x : xs) = go (x : acc) xs

concat :: ∀ a. List (List a) -> List a
concat Nil = Nil
concat (Nil : xss) = concat xss
concat ((x : xs) : xss) = x : concat (xs : xss)

filter :: ∀ a. (a -> Boolean) -> List a -> List a
filter _ Nil = Nil
filter p (x : xs) = if p x then x : filter p xs else filter p xs

filter' :: ∀ a. (a -> Boolean) -> List a -> List a
filter' _ Nil = Nil
filter' p (x : xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs

-- Point free tail recursive version
filter'' :: ∀ a. (a -> Boolean) -> List a -> List a
filter'' p = reverse <<< go Nil where
  go :: List a -> List a -> List a
  go acc Nil            = acc
  go acc (x : xs) | p x = go (x : acc) xs
  go acc (_ : xs)       = go acc xs

catMaybes :: ∀ a. List (Maybe a) -> List a
catMaybes Nil = Nil
catMaybes (Nothing : xs) = catMaybes xs
catMaybes (Just(x): xs)  = x : catMaybes xs

-- Or using "case"
catMaybes' :: ∀ a. List (Maybe a) -> List a
catMaybes' Nil = Nil
catMaybes' (x : xs) = case x of
  Just y  -> y : catMaybes xs
  Nothing -> catMaybes xs

-- This first attempt was rubbish
range' :: Int -> Int -> List Int
range' start end = reverse $ go Nil start end where
  go acc s e | s < e = go (s : acc) (s + 1) e
  go acc s e | s > e = go (s : acc) (s - 1) e
  go acc s _ = s : acc

range :: Int -> Int -> List Int
range start end | start == end = singleton start
range start end = start : range next end where
  next = start + (if start < end then 1 else (-1))

-- Alternative to calculate the "step" only once i.e. whether to increment or decrement (the above recalculates for every recursion)
range'' :: Int -> Int -> List Int
range'' start end = go start where
  step = if start < end then 1 else (-1)

  go :: Int -> List Int
  go s  | s == end  = singleton s
        | otherwise = s : go (s + step)

-- And the above as tail recursive:
range''' :: Int -> Int -> List Int
range''' start end = go Nil end where
  step = if start < end then (-1) else 1

  go :: List Int -> Int -> List Int
  go acc e  | e == start  = e : acc
            | otherwise   = go (e : acc) (e + step)

take :: ∀ a. Int -> List a -> List a
take _ Nil        = Nil
take n _ | n <= 0 = Nil
take n (x : xs)   = x : take (n - 1) xs

-- And tail recursive (we could have gone point free, but can't as we've introduced use of "max" which then avoids needing a guard)
take' :: ∀ a. Int -> List a -> List a
take' n xs = go Nil (max 0 n) xs where
  go :: List a -> Int -> List a -> List a
  go acc _ Nil      = acc
  go acc 0 _        = acc
  go acc n (x : xs) = go (concat (acc : singleton x : Nil)) (n - 1) xs

drop :: ∀ a. Int -> List a -> List a
drop n xs = go (max 0 n) xs where
  go :: Int -> List a -> List a
  go _ Nil      = Nil
  go 0 xs       = xs
  go n (_ : xs) = go (n - 1) xs

takeWhile :: ∀ a. (a -> Boolean) -> List a -> List a
takeWhile p xs = go Nil xs where
  go :: List a -> List a -> List a
  go acc (x : xs) | p x = go (concat (acc : singleton x : Nil)) xs
  go acc _ = acc

dropWhile :: ∀ a. (a -> Boolean) -> List a -> List a
dropWhile p (x : xs) | p x = dropWhile p xs
dropWhile _ xs = xs

{- 
takeEnd 2 (1 : 2 : 3 : 4 : Nil)

Tuple 0 Nil             -- base-case
Tuple 1 (4 : Nil)       -- returning
Tuple 2 (3 : 4 : Nil)   -- returning
Tuple 3 (3 : 4 : Nil)   -- returning
Tuple 4 (3 : 4 : Nil)   -- returning
(3 : 4 : Nil)           -- snd
-}
takeEnd :: ∀ a. Int -> List a -> List a
takeEnd n = go >>> snd where
  go Nil = Tuple 0 Nil
  go (x : xs) = go xs # \t @ (Tuple c nl) -> if c < n then Tuple (c + 1) (x : nl) else t 

dropEnd :: ∀ a. Int -> List a -> List a
dropEnd n = go >>> snd where
  go Nil = Tuple 0 Nil
  go (x : xs) = go xs # \(Tuple c nl) -> Tuple (c + 1) $ if c < n then nl else x : nl

zip :: ∀ a b. List a -> List b -> List (Tuple a b)
zip _ Nil = Nil
zip Nil _ = Nil
zip (a : as) (b : bs) = Tuple a b : zip as bs

unzip :: ∀ a b. List (Tuple a b) -> Tuple (List a) (List b)
unzip Nil = Tuple Nil Nil
unzip (Tuple a b : ts) = unzip ts # \(Tuple as bs) -> Tuple (a : as) (b : bs)

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

  log $ show $ concat ((1 : 2 : 3 : Nil) : (4 : 5 : Nil) : (6 : Nil) : Nil : Nil)

  log $ show $ filter (4 > _) $ (1 : 2 : 3 : 4 : 5 : 6 : Nil)

  log $ show $ catMaybes (Just 1 : Nothing : Just 2 : Nothing : Nothing : Just 5 : Nil)

  log $ show $ range 1 10
  
  log $ show $ range''' 1 10
  
  log $ show $ range 3 (-3)
  
  log $ show $ range''' 3 (-3)

  log $ show $ take 5 (12 : 13 : 14 : Nil)
  
  log $ show $ take' 5 (12 : 13 : 14 : Nil)
  
  log $ show $ take 5 (-7 : 9 : 0 : 12 : 13 : 45 : 976 : -19 : Nil)
  
  log $ show $ take' 5 (-7 : 9 : 0 : 12 : 13 : 45 : 976 : -19 : Nil)
  
  log $ show $ drop 2 (1 : 2 : 3 : 4 : 5 : 6 : 7 : Nil)
  
  log $ show $ drop 10 (Nil :: List Unit)
  
  log $ show $ takeWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil)
  
  log $ show $ takeWhile (_ == -17) (1 : 2 : 3 : Nil)
  
  log $ show $ dropWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil)
  
  log $ show $ dropWhile (_ == -17) (1: 2 : 3 : Nil)
  
  log $ show $ takeEnd 3 (1 : 2 : 3 : 4 : 5 : 6 : Nil)
  
  log $ show $ takeEnd 10 (1 : Nil)

  -- Prints (1 : 2 : 3 : Nil)
  log $ show $ dropEnd 3 (1 : 2 : 3 : 4 : 5 : 6 : Nil)

  -- Prints (Nil)
  log $ show $ dropEnd 10 (1 : Nil)

  log $ show $ zip (1 : 2 : 3 : Nil) ("a" : "b" : "c" : "d" : "e" : Nil)

  log $ show $ zip ("a" : "b" : "c" : "d" : "e" : Nil) (1 : 2 : 3 : Nil)

  log $ show $ zip (Nil :: List Unit) (1 : 2 : Nil)

  log $ show $ unzip (Tuple 1 "a" : Tuple 2 "b" : Tuple 3 "c" : Nil)

  log $ show $ unzip (Tuple "a" 1 : Tuple "b" 2 : Tuple "c" 3 : Nil)

  log $ show $ unzip (Nil :: List (Tuple Unit Unit))