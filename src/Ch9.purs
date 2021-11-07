{- Coding Abstract Algebra -}

module Ch9 where

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, class Show, class Eq, ($), discard, show, (==), (&&))

--------------------------------------------------------

class Semigroup a where
  append :: a -> a -> a

infixr 5 append as <>

class Semigroup a <= Monoid a where
  mempty :: a

--------------------------------------------------------

data AndBool = AFalse | ATrue

derive instance eqAndBool :: Eq AndBool

derive instance genAndBool :: Generic AndBool _

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

derive instance genOrBool :: Generic OrBool _

instance showOrBool :: Show OrBool where
  show = genericShow

instance semigroupOrBool :: Semigroup OrBool where
  append OFalse OFalse = OFalse
  append _ _ = OTrue

instance monoidOrBool :: Monoid OrBool where
  mempty = OFalse 

--------------------------------------------------------

data Mod4 = Zero | One | Two | Three

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