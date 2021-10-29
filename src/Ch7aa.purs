-- Coding Typeclasses - Deriving

module Ch7aa where

import Prelude (Unit, discard, (==), ($), (<), (>), (<=), (>=), (||))
import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Ord (class Ord)
import Data.Show (class Show, show)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)

-- -----------------------------------------------

data Maybe a = Nothing | Just a

derive instance eqMaybe :: Eq a => Eq (Maybe a)

derive instance ordMaybe :: Ord a => Ord (Maybe a)

derive instance genMaybe :: Generic (Maybe a) _

instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

-- -----------------------------------------------

data Either a b = Left a | Right b

derive instance eqEither :: (Eq a, Eq b) => Eq (Either a b)

derive instance ordEither :: (Ord a, Ord b) => Ord (Either a b)

derive instance genEither :: Generic (Either a b) _

instance showEither :: (Show a, Show b) => Show (Either a b) where
  show = genericShow

type MyEither = Either String (Maybe Int)  

-- -----------------------------------------------

test :: Effect Unit
test = do
  log $ show $ Just 5 == Just 5
  log $ show $ Just 5 == Just 2
  log $ show $ Just 5 == Nothing
  log $ show $ Nothing == Just 5
  log $ show $ Nothing == (Nothing :: Maybe Unit)
  log "--------------------------"
  log $ show $ Just 1 < Just 5
  log $ show $ Just 5 <= Just 5
  log $ show $ Just 5 > Just 10
  log $ show $ Just 10 >= Just 10
  log $ show $ Just 99 > Nothing
  log $ show $ Just 99 < Nothing
  log "--------------------------"
  log $ show $ Just "abc"  
  log $ show $ (Nothing :: Maybe Unit)  
  log "--------------------------"
  log $ show $ (Left "left" :: Either _ Unit)     -- Type inference works out _, though we could provide String  
  log $ show $ (Right (Just 42) :: Either Unit _) -- Type inference works out _, though we could provide (Maybe Int)
  let x = Left "left" :: MyEither
      y :: MyEither
      y = Right $ Just 42
  log $ show x    
  log $ show y    