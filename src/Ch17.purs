module Ch17 where

import Data.Bifunctor (class Bifunctor)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Prelude

data Maybe a = Nothing | Just a

derive instance genericMaybe :: Generic (Maybe a) _

instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

instance functorMaybe :: Functor Maybe where
  map :: ∀ a b. (a -> b) -> Maybe a -> Maybe b
  map _ Nothing   = Nothing
  map fn (Just a) = Just $ fn a

instance applyMaybe :: Apply Maybe where
  apply :: ∀ a b. Maybe (a -> b) -> Maybe a -> Maybe b
  apply (Just fn) m = fn <$> m -- map fn m
  apply Nothing _ = Nothing

instance applicativeMaybe :: Applicative Maybe where
  pure :: ∀ a. a -> Maybe a
  pure = Just  

------------------------------------

data Either a b = Left a | Right b

derive instance genericEither :: Generic (Either a b) _

instance showEither :: (Show a, Show b) => Show (Either a b) where
  show = genericShow

derive instance eqEither :: (Eq a, Eq b) => Eq (Either a b)

derive instance ordEither :: (Ord a, Ord b) => Ord (Either a b)

derive instance functorEither :: Functor (Either a)

{-
class Bifunctor f where
  bimap :: ∀ a b c d. (a -> c) -> (b -> d) -> f a b -> f c d
-}

instance bifunctorEither :: Bifunctor Either where
  bimap :: ∀ a b c d. (a -> c) -> (b -> d) -> Either a b -> Either c d
  bimap fac _ (Left a)  = Left $ fac a
  bimap _ fbd (Right b) = Right $ fbd b

{-
class Functor f <= Apply f where
  apply :: ∀ a b. f (a -> b) -> f a -> f b
-}

instance applyEither :: Apply (Either a) where
  apply :: ∀ b c. Either a (b -> c) -> Either a b -> Either a c
  apply (Right fbc) either = fbc <$> either
  apply (Left a) _ = Left a

{-
class Apply f <= Applicative f where
  pure :: ∀ a. a -> f a
-}

instance applicativeEither :: Applicative (Either a) where
  pure :: ∀ b. b -> Either a b
  pure = Right

------------------------------------

test :: Effect Unit
test = do
  log $ show $ map (_ + 3) (Just 4)
  log $ show $ (_ + 3) <$> (Just 4)
  log $ show $ apply (Just (_ + 3)) (Just 4)
  log $ show $ Just (_ + 3) <*> (Just 4)
  log $ show $ pure (_ + 3) <*> (Just 4)
  log "------------------------------------"
  log $ show $ (+) <$> Just 21 <*> Just 21                -- (Just 42)
  log $ show $ (*) <$> pure 2 <*> (pure 21 :: Maybe Int)  -- (Just 42)
  log $ show $ pure (+) <*> Just 17 <*> Just 25           -- (Just 42)
  log "------------------------------------"
  -- LAW Associative Composition: (<<<) <$> u <*> v <*> w = u <*> (v <*> w)
  log $ show $ ((<<<) <$> pure identity <*> pure identity <*> pure 1) == (pure identity <*> (pure identity <*> pure 1) :: Either Unit Int)
  -- LAW Identity: pure identity <*> x = x
  log $ show $ (pure identity <*> pure 1) == (pure 1 :: Either Unit Int)
  -- LAW Homomorphism: pure (f x) = pure f <*> pure x
  log $ show $ pure (negate 1) == (pure negate <*> pure 1 :: Either Unit Int)
  -- LAW Interchange: u <*> pure x = pure (_ $ x) <*> u
  log $ show $ (pure negate <*> pure 1) == (pure (_ $ 1) <*> pure negate :: Either Unit Int)