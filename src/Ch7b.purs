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

derive newtype instance showCSV :: Show CSV

class ToCSV a where
  toCSV :: a -> CSV

newtype FullName = FullName String

derive instance newtypeFullName :: Newtype FullName _

derive newtype instance eqFullName :: Eq FullName

instance showFullName :: Show FullName where
  show :: FullName -> String
  show (FullName n) = n

newtype Age = Age Int

derive instance newtypeAge :: Newtype Age _

derive newtype instance showAge :: Show Age

derive newtype instance eqAge :: Eq Age

data Occupation = Doctor | Dentist | Lawyer | Unemployed

derive instance genericOccupation :: Generic Occupation _

instance showOccupation :: Show Occupation where
  show = genericShow

derive instance eqOccupation :: Eq Occupation 

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

derive instance eqPerson :: Eq Person

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

test :: Effect Unit
test = do
  let person = Person {
    name: FullName "scooby",
    age: Age 22,
    occupation: Doctor
  }
  log $ show $ toCSV person
  log $ show $ toCSV person == CSV "scooby, 22, Doctor"
  log $ show $ (toCSV person # fromCSV) == Just person  