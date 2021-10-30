module Ch7b where

import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Console (log)
import Prelude

newtype CSV = CSV String

derive instance newtypeCSV :: Newtype CSV _

derive newtype instance eqCSV :: Eq CSV

class ToCSV a where
  toCSV :: a -> CSV

newtype FullName = FullName String

instance showFullName :: Show FullName where
  show :: FullName -> String
  show (FullName n) = n

newtype Age = Age Int

derive instance newtypeAge :: Newtype Age _

derive newtype instance showAge :: Show Age

data Occupation = Doctor | Dentist | Lawyer | Unemployed

derive instance genOccupation :: Generic Occupation _

instance showOccupation :: Show Occupation where
  show = genericShow

toOccupation :: String -> Maybe Occupation
toOccupation "Doctor" = Just Doctor
toOccupation "Dentist" = Just Dentist
toOccupation "Lawyer" = Just Lawyer
toOccupation "Unemployed" = Just Unemployed
toOccupation _ = Nothing

data Person =
  Person {
    name :: FullName,
    age :: Age,
    occupation :: Occupation
  }

instance toCSVPerson :: ToCSV Person where
  toCSV :: Person -> CSV
  toCSV (Person { name, age, occupation }) = CSV $ show name <> ", " <> show age <> ", " <> show occupation

------------------------------

class FromCSV a where
  fromCSV :: CSV -> Maybe a
  
{- instance fromCSVPerson :: FromCSV Person where
  fromCSV :: CSV -> Maybe Person
  fromCSV (CSV csv) = case split (Pattern ", ") csv of
    [name, age, occupation] -> (fromString age) >>= \age -> (toOccupation occupation) >>= \occupation -> Just $ Person { name: FullName name, age: Age age, occupation: occupation }
    _ -> Nothing -}

instance fromCSVPerson :: FromCSV Person where
  fromCSV :: CSV -> Maybe Person
  fromCSV (CSV csv) = case split (Pattern ", ") csv of
    [name, age, occupation] -> do
      age <- fromString age
      occupation <- toOccupation occupation
      pure $ Person { name: FullName name, age: Age age, occupation: occupation }
    _ -> Nothing    

------------------------------

person :: Person
person = Person {
  name: FullName "scooby",
  age: Age 22,
  occupation: Doctor
}

test :: Effect Unit
test = do
  log $ show $ toCSV person == CSV "scooby, 22, Doctor"