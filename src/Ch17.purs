module Ch17

(
  Age (..),         -- Export both Type Constructor and Data Constructor using (..)
  FamilyAges,       -- Only export Type Constructor meaning you cannot construct the Type directly (we want to force the client to use our smart constructor createFamilyAges)
  FamilyAgesRow,    -- Export Type Alias
  Validation,       -- Upon calling smart constructor, client get back a Validation, so needs to know about this type, so again we do not export the Data Constructor
  createFamilyAges, -- Our smart constructor
  test
) where

import Data.Bifunctor (class Bifunctor)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
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
{-
Validation

========================================================================================

fullNameEither :: Maybe String -> Maybe String -> Maybe String -> Either String String
fullNameEither first middle last =
  fullName <$> (first `errIfMissing` "First name must exist")
          <*> (middle `errIfMissing` "Middle name must exist")
          <*> (last `errIfMissing` "Last name must exist")

========================================================================================
Explanation:


fullName :: String -> String -> String -> String

errIfMissing :: Maybe String -> String -> Either String String


fullNameEither :: Maybe String -> Maybe String -> Maybe String -> Either String String
fullNameEither first middle last =
  fullName <$> (first `errIfMissing` "First name must exist") ...

 
psuedo:

(first `errIfMissing` "First name must exist") map fullName

((Just "Bob") `errIfMissing` "First name must exist") map fullName

(`errIfMissing`(Just "Bob")("First name must exist") map fullName

(Right "Bob") map fullName

(Right "Bob" -> String -> String -> String)

i.e.

fullName <$> (first `errIfMissing` "First name must exist") =:= (Right "Bob" -> String -> String -> String)
																Either String (String -> String -> String)

At this point, we now have a Partially Applied function in a Context, viz. Either. Notice that it’s waiting for 2 more Strings, i.e. middle and last.
So next we use "apply" aka <*>

That's all fine except...
Since this returns an Either, which has an Explicit Behavior (or Effect) of Short-circuiting, we only get the first error.
In the above, if both first and last are missing, we’d never know that.
-}

newtype Validation err result = Validation (Either err result)

derive newtype instance functorValidation :: Functor (Validation err)

derive newtype instance bifunctorValidation :: Bifunctor Validation

derive instance newtypeValidation :: Newtype (Validation err result) _

derive instance eqValidation :: (Eq err, Eq result) => Eq (Validation err result)

derive instance ordValidation :: (Ord err, Ord result) => Ord (Validation err result)

derive instance genericValidation :: Generic (Validation err result) _

instance showValidation :: (Show err, Show result) => Show (Validation err result) where
  show = genericShow

-- The next step is the whole reason for this Type. We want to write an Apply Instance that collects all of the errors in some sort of Semigroup or Monoid.

instance applyValidation :: Semigroup err => Apply (Validation err) where
  apply :: ∀ b c. Validation err (b -> c) -> Validation err b -> Validation err c
  apply (Validation (Left e1)) (Validation (Left e2)) = Validation $ Left (e1 <> e2)
  apply (Validation (Left e1)) _ = Validation $ Left e1
  apply (Validation (Right fbc)) validation = fbc <$> validation

instance applicativeValidation :: Semigroup err => Applicative (Validation err) where
  pure :: ∀ b. b -> Validation err b
  pure = Validation <<< Right

{-
Note:
We still need to constrain the error Type to Semigroup since an Applicative is also an Apply, which has this constraint. If you forget this, the compiler will dutifully remind you.
-}  

------------------------------------
{-
Using Validation

Validate a family of Father, Mother and Child all with names and ages.

First a recap on records and rows:
-}

type Address = {
  street :: String,
  city :: String,
  state :: String,
  zip :: Int
}

isCalifornia :: ∀ r. { address :: Address | r } -> Boolean
isCalifornia { address: { state } } = state == "CA"

{-
The salient portion here is the | r portion. That tells us that we can have other fields in this Record.

type FamilyAgesRow = (fatherAge :: Int, motherAge :: Int, childAge :: Int)

Notice the syntax for a Row is different than for a Record. We use Parentheses instead of Curly Brackets.

Now here is a "Extensible Record":

type Family r = { -- We have to pass r as the Row that will Extend this Record, hence the term Extensible Record.
  fatherName :: String,
  motherName :: String,
  childName :: String
  | r -- We specify that r is a Row Type that extends the Record using this syntax.
}

When creating a Family, e.g we could add other fields e.g.:
Family (fatherOccupation :: Occupation, motherOccupation :: Occupation)

Or we could add NO fields:
Family ()
-}

newtype Age = Age Int

derive instance genericAge :: Generic Age _

instance showAge :: Show Age where
  show = genericShow

newtype FullName = FullName String

derive instance genericFullName :: Generic FullName _

instance showFullName :: Show FullName where
  show = genericShow

------
-- Side note: The above would be printed different to the following:
newtype Age' = Age' Int

derive newtype instance showAge' :: Show Age'

ageDerivedFromGeneric = show $ Age 10 -- If we log we'll see: (Age 10)

ageDerivedDirect = show $ Age' 10     -- If we log we'll see: 10

-- With "derive newtype", the compiler just delegates to the underlying Type

------  

type FamilyAgesRow r = (fatherAge :: Age, motherAge :: Age, childAge :: Age | r)

type FamilyNamesRow r = (fatherName :: FullName, motherName :: FullName, childName :: FullName | r)

newtype Family = Family { | FamilyNamesRow (FamilyAgesRow ()) }
{-
which can be written as:
newtype Family = Family (Record (FamilyNamesRow (FamilyAgesRow ())))

We have the newtype called Family that has a Data Constructor that takes a Record which has all of the fields in FamilyNamesRow and FamilyAgesRow.
-}

derive instance genericFamily :: Generic Family _

instance showFamily :: Show Family where
  show = genericShow

newtype FamilyAges = FamilyAges { | FamilyAgesRow () }

derive instance genericFamilyAges :: Generic FamilyAges _

instance showFamilyAges :: Show FamilyAges where
  show = genericShow

-- Check to make sure that the age is in a specified range
newtype LowerAge = LowerAge Int
newtype UpperAge = UpperAge Int

validateAge :: LowerAge -> UpperAge -> Age -> String -> Validation (Array String) Age
validateAge (LowerAge lowerAge) (UpperAge upperAge) a @ (Age age) who
  | age < lowerAge  = Validation $ Left [who <> " is too young"]
  | age > upperAge  = Validation $ Left [who <> " is too old"]
  | otherwise       = Validation $ Right a

createFamilyAges :: { | FamilyAgesRow () } -> Validation (Array String) FamilyAges
createFamilyAges { fatherAge, motherAge, childAge } =
  FamilyAges <$> (
    { fatherAge: _, motherAge: _, childAge: _ } -- Shorthand for the lambda function: \fatherAge motherAge childAge -> { fatherAge: fatherAge, motherAge: motherAge, childAge: childAge }
    <$> (validateAge (LowerAge 18) (UpperAge 60) fatherAge "Father")
    <*> (validateAge (LowerAge 18) (UpperAge 60) motherAge "Mother")
    <*> (validateAge (LowerAge 1) (UpperAge 18) childAge "Child")
  )

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
  log "------------------------------------"
  log ageDerivedFromGeneric
  log ageDerivedDirect
  log "------------------------------------"
  log $ show $ createFamilyAges { fatherAge: Age 40, motherAge: Age 30, childAge: Age 10 }
  log $ show $ createFamilyAges { fatherAge: Age 400, motherAge: Age 300, childAge: Age 0 }
  log $ show $ createFamilyAges { fatherAge: Age 4, motherAge: Age 3, childAge: Age 10 }
  log $ show $ createFamilyAges { fatherAge: Age 40, motherAge: Age 30, childAge: Age 100 }
  log $ show $ createFamilyAges { fatherAge: Age 40, motherAge: Age 3, childAge: Age 0 }