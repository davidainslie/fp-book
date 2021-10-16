module Ch5 where

import Prelude (Unit, show, discard)
import Data.List (List(..), (:)) -- List(..) is shorthand for List(Nil, Cons) i.e. all data constructors
import Effect (Effect)
import Effect.Console (log)

flip :: ∀ a b c. (a -> b -> c) -> b -> a -> c
flip f b a = f a b

-- If we included parentheses, our eyes would trick us into writing another variation
flip2 :: ∀ a b c. (a -> b -> c) -> (b -> a -> c)
flip2 f = \ x y -> f y x

const :: ∀ a b. a -> b -> a
const a _ = a

apply :: ∀ a b. (a -> b) -> a -> b
apply f a = f a

infixr 0 apply as $

applyFlipped :: ∀ a b. a -> (a -> b) -> b
applyFlipped = flip apply

infixl 1 applyFlipped as #

-- -----------------------------------------------

test :: Effect Unit
test = do
  log (show (flip const 1 2))

test2 :: Effect Unit
test2 = do
  log $ show $ flip const 1 2
  flip const 1 2 # show # log 