{- Coding Abstract Algebra -}

module Ch9 where

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show (class Show, show)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, class Show, class Eq, ($), discard, (==), (&&))

--------------------------------------------------------

class Semigroup a where
  append :: a -> a -> a

infixr 5 append as <>

class Semigroup a <= Monoid a where
  mempty :: a

--------------------------------------------------------

data AndBool = AFalse | ATrue

derive instance eqAndBool :: Eq AndBool

derive instance genericAndBool :: Generic AndBool _

instance showAndBool :: Show AndBool where
  show = genericShow

instance semigroupAndBool :: Semigroup AndBool where
  append ATrue ATrue = ATrue
  append _ _ = AFalse

instance monoidAndBool :: Monoid AndBool where
  mempty = ATrue   

--------------------------------------------------------

data OrBool = OFalse | OTrue

derive instance eqOrBool :: Eq OrBool

derive instance genericOrBool :: Generic OrBool _

instance showOrBool :: Show OrBool where
  show = genericShow

instance semigroupOrBool :: Semigroup OrBool where
  append OFalse OFalse = OFalse
  append _ _ = OTrue

instance monoidOrBool :: Monoid OrBool where
  mempty = OFalse 

--------------------------------------------------------

data Mod4 = Zero | One | Two | Three

derive instance eqMod4 :: Eq Mod4

derive instance genericMod4 :: Generic Mod4 _

instance showMod4 :: Show Mod4 where
  show = genericShow

instance semigroupMod4 :: Semigroup Mod4 where
  append Zero x = x
  append x Zero = x
  append One One = Two
  append One Two = Three
  append Two One = Three
  append One Three = Zero
  append Three One = Zero
  append Two Two = Zero
  append Two Three = One
  append Three Two = One
  append Three Three = Two

instance monoidMod4 :: Monoid Mod4 where
  mempty = Zero  

--------------------------------------------------------

class Monoid a <= Group a where
  ginverse :: a -> a

instance groupMod4 :: Group Mod4 where
  ginverse Zero = Zero
  ginverse One = Three
  ginverse Two = Two
  ginverse Three = One  

--------------------------------------------------------

class Semigroup g <= Commutative g

instance commutativeMod4 :: Commutative Mod4

--------------------------------------------------------

newtype First a = First (Maybe a)

newtype Last a = Last (Maybe a)

-- Note we take the type i.e. newtype First a, so take "First a" and that forms the instance signature
instance semigroupFirst :: Semigroup (First a) where
  append (First Nothing) x = x
  append x _ = x

instance monoidFirst :: Monoid (First a) where
  mempty = First Nothing

-- The following would only show e.g. (Just 77) but we want to see e.g. (First (Just 77))
-- derive newtype instance firstShow :: Show a => Show (First a)

derive instance genericFirst :: Generic (First a) _

instance showFirst :: Show a => Show (First a) where
  show = genericShow

instance semigroupLast :: Semigroup (Last a) where
  append x (Last Nothing) = x
  append _ x = x

instance monoidLast :: Monoid (Last a) where
  mempty = Last Nothing

-- derive newtype instance lastShow :: Show a => Show (Last a)

derive instance genericLast :: Generic (Last a) _

instance showLast :: Show a => Show (Last a) where
  show = genericShow

--------------------------------------------------------

test :: Effect Unit
test = do
  log $ show $ ATrue <> ATrue
  log $ show $ ATrue <> AFalse
  log $ show $ AFalse <> AFalse
  -----------------------------
  log $ show $ mempty <> ATrue == ATrue
  log $ show $ mempty <> AFalse == ATrue
  -----------------------------
  verifyAndBoolSemigroup
  verifyAndBoolMonoid
  -----------------------------
  verifyOrBoolSemigroup
  verifyOrBoolMonoid
  -----------------------------
  verifyMod4Semigroup
  verifyMod4Monoid
  -----------------------------
  log $ show $ First Nothing <> First (Just 77)
  log $ show $ Last (Just 1) <> Last (Just 99)

verifyAndBoolSemigroup :: Effect Unit
verifyAndBoolSemigroup = do
  log "Verifying AndBool Semigroup Laws (1 test)"
  log $ show $ AFalse <> (ATrue <> ATrue) == (AFalse <> ATrue) <> ATrue

verifyAndBoolMonoid :: Effect Unit
verifyAndBoolMonoid = do
  log "Verifying AndBool Monoid Laws (2 tests)"
  log $ show $ ATrue <> mempty == mempty <> ATrue && ATrue <> mempty == ATrue
  log $ show $ AFalse <> mempty == mempty <> AFalse && AFalse <> mempty == AFalse

verifyOrBoolSemigroup :: Effect Unit
verifyOrBoolSemigroup = do
  log "Verifying OrBool Semigroup Laws (1 test)"
  log $ show $ (OFalse <> OTrue) <> OTrue == OFalse <> (OTrue <> OTrue)

verifyOrBoolMonoid :: Effect Unit
verifyOrBoolMonoid = do
  log "Verifying OrBool Monoid Laws (2 tests)"
  log $ show $ OTrue <> mempty == mempty <> OTrue && OTrue <> mempty == OTrue
  log $ show $ OFalse <> mempty == mempty <> OFalse && OFalse <> mempty == OFalse

verifyMod4Semigroup :: Effect Unit
verifyMod4Semigroup = do
  log "Verifying Mod4 Semigroup Laws (1 test)"
  log $ show $ (Zero <> One) <> Two == Zero <> (One <> Two)

verifyMod4Monoid :: Effect Unit
verifyMod4Monoid = do
  log "Verifying Mod4 Monoid Laws (3 tests)"
  log $ show $ One <> mempty == mempty <> One && One <> mempty == One
  log $ show $ Two <> mempty == mempty <> Two && Two <> mempty == Two
  log $ show $ Three <> mempty == mempty <> Three && Three <> mempty == Three