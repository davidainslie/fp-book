-- Coding Functors

module Ch13 where

import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.Common (toUpper)
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Show, Unit, show, discard, ($), (/), (<>), (==), identity, (<<<), (*))
-- Remembering that "discard" is for "do syntax"

-----------------------------------------------

class Functor f where
  map :: ∀ a b. (a -> b) -> f a -> f b

infixl 4 map as <$>  
  
-----------------------------------------------

data Maybe a = Nothing | Just a -- Nothing defined first so that it sorts before Just

derive instance genericMaybe :: Generic (Maybe a) _

instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

derive instance eqMaybe :: Eq a => Eq (Maybe a)

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

data Tuple a b = Tuple a b

derive instance genericTuple :: Generic (Tuple a b) _

instance showTuple :: (Show a, Show b) => Show (Tuple a b) where
  show = genericShow

derive instance eqTuple :: (Eq a, Eq b) => Eq (Tuple a b)  

instance functorTuple :: Functor (Tuple a) where
  map :: ∀ b c. (b -> c) -> Tuple a b -> Tuple a c
  map fn (Tuple a b) = Tuple a $ fn b

-----------------------------------------------

data Threeple a b c = Threeple a b c

derive instance genericThreeple :: Generic (Threeple a b c) _

instance showThreeple :: (Show a, Show b, Show c) => Show (Threeple a b c) where
  show = genericShow

instance functorThreeple :: Functor (Threeple a b) where
  map :: ∀ c d. (c -> d) -> Threeple a b c -> Threeple a b d
  map fn (Threeple a b c) = Threeple a b $ fn c

-----------------------------------------------

class Bifunctor f where
  bimap :: ∀ a b c d. (a -> c) -> (b -> d) -> f a b -> f c d

{- 
Bifunctor Laws

Identity:
bimap identity identity = identity

Composition:
bimap (g1 <<< f1) (g2 <<< f2) = bimap g1 g2 <<< bimap f1 f2
-}  

rmap :: ∀ f a b c. Bifunctor f => (b -> c) -> f a b -> f a c
rmap = bimap identity

lmap :: ∀ f a b c. Bifunctor f => (a -> c) -> f a b -> f c b
lmap fn = bimap fn identity
-- To be completely point free we would need to use "flip"
-- lmap = flip bimap identity

instance bifunctorEither :: Bifunctor Either where
  bimap :: ∀ a b c d. (a -> c) -> (b -> d) -> Either a b -> Either c d
  bimap fn _ (Left a) = Left $ fn a
  bimap _ fn (Right b) = Right $ fn b

instance bifunctorTuple :: Bifunctor Tuple where
  bimap :: ∀ a b c d. (a -> c) -> (b -> d) -> Tuple a b -> Tuple c d
  bimap f g (Tuple a b) = Tuple (f a) (g b)

instance bifunctorThreeple :: Bifunctor (Threeple a) where
  bimap :: ∀ b c bb cc. (b -> bb) -> (c -> cc) -> (Threeple a b c) -> (Threeple a bb cc)
  bimap f g (Threeple a b c) = Threeple a (f b) (g c)

{-  
Bifunctor Laws... by substitution

  Identity:

    Given:
    bimap identity identity (Tuple x y)   = identity (Tuple x y)

    Substitution:
    Tuple (identity x) (identity y)       = identity (Tuple x y)

    Function application:
    Tuple x y                             = identity (Tuple x y)

    Function application:
    Tuple x y                             = Tuple x y

  Composition:

    Given:
    bimap (g1 <<< f1) (g2 <<< f2) (Tuple x y)     = bimap g1 g2 <<< bimap f1 f2 (Tuple x y)

    Substitution:
    Tuple (g1 <<< f1 $ x) (g2 <<< f2 $ y)         = bimap g1 g2 <<< bimap f1 f2 (Tuple x y)

    Compose equivalence:
    Tuple (g1 (f1 x)) (g2 (f2 y))                 = bimap g1 g2 <<< bimap f1 f2 (Tuple x y)

    Compose equivalence:
    Tuple (g1 (f1 x)) (g2 (f2 y))                 = bimap g1 g2 (bimap f1 f2 (Tuple x y))

    Substitution:
    Tuple (g1 (f1 x)) (g2 (f2 y))                 = bimap g1 g2 (Tuple (f1 x) (f2 y))

    Substitution:
    Tuple (g1 (f1 x)) (g2 (f2 y))                 = Tuple (g1 (f1 x)) (g2 (f2 y))
-}

-----------------------------------------------

test :: Effect Unit
test = do
  log $ show $ (_ / 2) <$> Just 10
  log $ show $ (_ / 2) <$> Nothing

  log $ show $ (_ / 2) <$> (Right 10 :: Either Unit _)
  log $ show $ (_ / 2) <$> (Left "Error reason" :: Either _ Int)

  log $ show $ (_ / 2) <$> Tuple 10 20
  log $ show $ (_ / 2) <$> Threeple 10 20 40

  -- Functor Laws
  log $ show $ "Maybe Identity for Nothing: " <> show ((identity <$> Nothing) == (Nothing :: Maybe Unit))
  log $ show $ "Maybe Identity for Just:    " <> show ((identity <$> Just [1, 2]) == Just [1, 2])

  let g x = x * 2
      f x = x * 3
  log $ show $ "Maybe Composition for Nothing: " <> show ((map (g <<< f) Nothing) == (map g <<< map f) Nothing)
  log $ show $ "Maybe Composition for Just:    " <> show ((map (g <<< f) (Just 60)) == (map g <<< map f) (Just 60))

  log $ show $ "Tuple Identity:    " <> show ((identity <$> Tuple 10 20) == Tuple 10 20)
  log $ show $ "Tuple Composition: " <> show ((map (g <<< f) (Tuple 10 20)) == (map g <<< map f) (Tuple 10 20))

  -- Bifunctor
  log $ show $ rmap (_ * 2) $ Left "Error reason"
  log $ show $ rmap (_ * 2) $ (Right 10 :: Either Unit _)
  log $ show $ lmap toUpper $ (Left "Error reason" :: Either _ Unit)
  log $ show $ lmap toUpper $ Right 10

  log $ show $ rmap (_ * 2) $ Tuple 80 40
  log $ show $ lmap (_ / 2) $ Tuple 80 40
  log $ show $ bimap (_ / 2) (_ * 2) $ Tuple 80 40

  log $ show $ rmap (_ * 2) $ Threeple 99 80 40
  log $ show $ lmap (_ / 2) $ Threeple 99 80 40
  log $ show $ bimap (_ / 2) (_ * 2) $ Threeple 99 80 40