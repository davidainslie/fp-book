-- Coding Functors

module Ch13 where

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Show, Unit, show, discard, ($), (/))
-- Remembering that "discard" if for "do syntax"

-----------------------------------------------

class Functor f where
  map :: ∀ a b. (a -> b) -> f a -> f b

infixl 4 map as <$>  
  
-----------------------------------------------

data Maybe a = Nothing | Just a -- Nothing defined first so that it sorts before Just

derive instance genericMaybe :: Generic (Maybe a) _

instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

{- 
NOTICE we do not specify an "a" as in "Functor Maybe a".
It's because "f's" Kind is "Type -> Type" which matches that of Maybe.
-}
instance functorMaybe :: Functor Maybe where
  map :: ∀ a b. (a -> b) -> Maybe a -> Maybe b
  map _ Nothing = Nothing
  map fn (Just a) = Just $ fn a

-----------------------------------------------

data Either a b = Left a | Right b

derive instance genericEither :: Generic (Either a b) _

instance showEither :: (Show a, Show b) => Show (Either a b) where
  show = genericShow

instance functorEither :: Functor (Either a) where
  map :: ∀ b c. (b -> c) -> Either a b -> Either a c
  map _ (Left a) = Left a
  map fn (Right b) = Right (fn b)

-----------------------------------------------

test :: Effect Unit
test = do
  log $ show $ (_ / 2) <$> Just 10
  log $ show $ (_ / 2) <$> Nothing

  log $ show $ (_ / 2) <$> (Right 10 :: Either Unit _)
  log $ show $ (_ / 2) <$> (Left "Error reason" :: Either _ Int)
