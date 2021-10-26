-- Typeclasses

module Ch6 where

import Prelude
import Effect (Effect)
import Effect.Console (log)

type Address = {
  street1 :: String,
  street2 :: String,
  city :: String,
  state :: String,
  zip :: String
}

data Person =
  Person {
    name :: String,
    age :: Int,
    address :: Address
  }

data Company =
  Company {
    name :: String,
    address :: Address
  }

data Residence =
  Home Address |
  Facility Address

data EmptyLot =
  EmptyLot {
    daysEmpty :: Int,
    price :: Int,
    address :: Address
  }

person :: Person
person = Person {
  name: "Joe Mama",
  age: 22,
  address: {
    street1: "123 Main Street",
    street2: "Apt 152",
    city: "Jamestown",
    state: "CA",
    zip: "95327"
  }
}

instance personShow :: Show Person where
  show (Person p) = p.name

class HasAddress a where
  getAddress :: a -> Address

instance personHasAddress :: HasAddress Person where
  getAddress (Person p) = p.address

instance companyHasAddress :: HasAddress Company where
  getAddress (Company c) = c.address

instance residenceHasAddress :: HasAddress Residence where
  getAddress (Home a) = a
  getAddress (Facility a) = a

instance emptyLotAddress :: HasAddress EmptyLot where
  getAddress (EmptyLot e) = e.address

newtype Directions = Directions Int

getDirections :: ∀ a. HasAddress a => a -> Directions
getDirections hasAddress =
  let address = getAddress hasAddress in
    Directions 101

{- 
Multiple constraints can be declared in 2 different ways:

getDirections :: ∀ a. Show a => HasAddress a => a -> Directions

getDirections :: ∀ a. (Show a, HasAddress a) => a -> Directions
-}

-- -----------------------------------------------

test :: Effect Unit
test = do
  log $ show person 