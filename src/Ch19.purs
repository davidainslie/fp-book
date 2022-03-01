module Ch19 where

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Prelude

-----------------------------------------------------
-- Maybe Monad

data Maybe a = Nothing | Just a

derive instance genericMaybe :: Generic (Maybe a) _

instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

instance functorMaybe :: Functor Maybe where
  map :: ∀ a b. (a -> b) -> Maybe a -> Maybe b
  map fab (Just a) = Just $ fab a
  map _ Nothing    = Nothing

instance applyMaybe :: Apply Maybe where
  apply :: ∀ a b. Maybe (a -> b) -> Maybe a -> Maybe b
  apply (Just fab) maybe = fab <$> maybe
  -- apply (Just fab) maybe = map fab maybe <-- Alternative
  apply Nothing _ = Nothing

instance applicativeMaybe :: Applicative Maybe where
  pure :: ∀ a. a -> Maybe a
  pure = Just

instance bindMaybe :: Bind Maybe where
  bind :: ∀ a b. Maybe a -> (a -> Maybe b) -> Maybe b
  bind (Just a) famb = famb a
  bind Nothing _ = Nothing

instance monadMaybe :: Monad Maybe  

-----------------------------------------------------
-- Either Monad

data Either a b = Left a | Right b

derive instance genericEither :: Generic (Either a b) _

instance showEither :: (Show a, Show b) => Show (Either a b) where
  show = genericShow

derive instance functorEither :: Functor (Either a)

instance applyEither :: Apply (Either a) where
  apply :: ∀ b c. Either a (b -> c) -> Either a b -> Either a c
  apply (Right fbc) either = fbc <$> either -- OR map fbc either
  apply (Left a) _ = Left a

instance applicativeEither :: Applicative (Either a) where
  pure :: ∀ b. b -> Either a b
  pure = Right

instance bindEither :: Bind (Either a) where
  bind :: ∀ b c. Either a b -> (b -> Either a c) -> Either a c
  bind (Right b) fbec = fbec b
  bind (Left a) _ = Left a

instance monadEither :: Monad (Either a)  

-----------------------------------------------------

test :: Effect Unit
test = do
  log $ show $ Just (_ * 10) <*> Just 20                          -- Prints: Just 200
  log $ show $ Just (_ * 10) <*> pure 20                          -- Prints: Just 200
  log $ show $ Just 20 >>= pure <<< (_ * 10)                      -- Prints: Just 200
  log $ show do
    x <- Just 20
    let y = x * 10
    pure y -- Prints: Just 200
  log $ show $ Just 20 >>= const Nothing >>= \y -> Just $ y + 42  -- Prints: Nothing
  log $ show do
    _ <- Just 20
    y <- Nothing
    pure $ y + 42 -- Prints: Nothing
  log "-----------------------------------------------"
  log $ show $ Right (_ * 10) <*> (Right 20 :: Either Unit _)
  log $ show $ Right (_ * 10) <*> (pure 20 :: Either Unit _)
  log $ show $ (Right 20 :: Either Unit _) >>= pure <<< (_ * 10)
  log $ show do
    x <- Right 20 :: Either Unit _
    let y = x * 10
    pure y
  log $ show $ Right 20 >>= const (Left "error") >>= \y -> Right $ y + 42
  log $ show do
    _ <- Right 20
    y <- Left "error"
    pure $ y + 42