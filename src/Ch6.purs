-- Typeclasses

module Ch6 where

import Prelude
import Data.Array (sort)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)

{- 
Address is just an alias for Record, instead of being a full blown data type.
But note:
Records for example can only be compared for equality if and only if all of their field types have Eq instances.
-}
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

instance eqPerson :: Eq Person where
  eq (Person p1) (Person p2) =
    p1.name == p2.name && p1.age == p2.age && p1.address == p2.address

-- Ord instance for Unit (already defined, but let's try our own, where we see that Ord is constrained by Eq)
newtype MyUnit = MyUnit Unit

instance eqMyUnit :: Eq MyUnit where
  eq _ _ = true

instance ordMyUnit :: Ord MyUnit where
  compare _ _ = EQ


data Place = First | Second | Third

instance eqPlace :: Eq Place where
  eq First First = true
  eq Second Second = true
  eq Third Third = true
  eq _ _ = false

instance ordPlace :: Ord Place where
  compare First First = EQ 
  compare Second Second = EQ 
  compare Third Third = EQ 
  compare First _ = LT
  compare Second First = GT
  compare Second Third = LT
  compare Third _ = GT

x :: Array Place
x = [Third, First, Second] 

sx :: Array Place
sx = sort x -- [First, Second, Third]

-- Deriving

data SomeType = This | That | TheOther | AndYetAnother

derive instance eqSomeType :: Eq SomeType

derive instance ordSomeType :: Ord SomeType

-- Generic deriving

derive instance genericSomeType :: Generic SomeType _

instance showSomeType :: Show SomeType where
  show = genericShow

-- Manual vs generic:

{-
Manual:

instance showPlace :: Show Place where
  show First  = "First"
  show Second = "Second"
  show Third  = "Third"
-}

-- Generic
derive instance genericPlace :: Generic Place _

instance showPlace :: Show Place where
  show = genericShow


-- Newtype typeclass

newtype FirstName = FirstName String

derive instance newTypeFirstName :: Newtype FirstName _

derive instance eqFirstName :: Eq FirstName

newtype LastName = LastName String

derive instance newTypeLastName :: Newtype LastName _

-- Unwrap via pattern patching
fullName :: FirstName -> LastName -> String
fullName (FirstName first) (LastName last) = first <> " " <> last

-- Unwrap via Newtype's method
fullName' :: FirstName -> LastName -> String
fullName' first last = unwrap first <> " " <> unwrap last

-- Example of when the Newtype typeclass comes in handy:

glueNames :: ∀ a b.
  Newtype a String => Newtype b String =>
  String -> a -> b -> String
glueNames between n1 n2 = unwrap n1 <> between <> unwrap n2

lastNameFirst :: LastName -> FirstName -> String
lastNameFirst = glueNames ", "

fullName'' :: FirstName -> LastName -> String
fullName'' = glueNames " "


-- Coming back to HasAddress, we replace data Person with newtype Person
newtype Person' =
  Person' {
    name :: String,
    age :: Int,
    address :: Address
  }

instance hasAddressPerson' :: HasAddress Person' where
  getAddress (Person' p) = p.address

newtype Ceo = Ceo Person'

newtype Janitor = Janitor Person'

{-
Manual:

instance hasAddressCeo :: HasAddress Ceo where
  getAddress (Ceo p) = getAddress p

instance hasAddressJanitor :: HasAddress Janitor where
  getAddress (Janitor p) = getAddress p
-}


{-
Semi Generic:

derive instance newtypeCeo :: Newtype Ceo _

derive instance newtypeJanitor :: Newtype Janitor _

genericPersonHasAddress :: ∀ a. Newtype a Person => a -> Address
genericPersonHasAddress wrappedPerson = getAddress $ unwrap wrappedPerson

instance hasAddressCeo :: HasAddress Ceo where
  getAddress = genericPersonHasAddress

instance hasAddressJanitor :: HasAddress Janitor where
  getAddress = genericPersonHasAddress
-}

-- Fully Generic
derive instance newtypeCeo :: Newtype Ceo _

derive newtype instance hasAddressCeo :: HasAddress Ceo

derive instance newtypeJanitor :: Newtype Janitor _

derive newtype instance hasAddressJanitor :: HasAddress Janitor


-- The following is an example of Scala ambiguous implicits
class Combine a where
  combine :: a -> a -> a

-- We want an instance to add Ints:
instance combineInt :: Combine Int where
  combine = (+)

-- But if we wanted to multiple Ints another time we can't just:
{-
instance combineMultipleInt :: Combine Int where
  combine = (*)
-}

-- A solution is to use newtype
newtype AddInt = AddInt Int

newtype MultipleInt = MultipleInt Int

instance combineAddInt :: Combine AddInt where
  combine (AddInt x) (AddInt y) = AddInt (x + y)

instance combineMultipleInt :: Combine MultipleInt where
  combine (MultipleInt x) (MultipleInt y) = MultipleInt (x + y)

-- Instance chaining allows to have a specific instance then fallback onto other instances:
class IsRecord a where
  isRecord :: a -> Boolean

instance isRecordRecord :: IsRecord (Record a) where
  isRecord _ = true
else instance isRecordOther :: IsRecord a where
  isRecord _ = false

-- Typeclass RULEs - instances must be either:
-- 1. Defined in the same module as the typeclass
-- 2. Defined in the same module as the type

-- If the above is not followed, we then have an Orphaned Instance.

-- -----------------------------------------------

test :: Effect Unit
test = do
  log $ show person