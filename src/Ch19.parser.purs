module Ch19Parser where

import Control.Alt (class Alt, (<|>))
import Control.Lazy (class Lazy, defer)
import Control.Plus (class Plus, empty)
import Data.Array ((:))
import Data.CodePoint.Unicode (isAlpha, isDecDigit)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty (NonEmpty, (:|), fromNonEmpty)
import Data.Show.Generic (genericShow)
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits (fromCharArray, singleton, uncons)
import Data.Traversable (class Traversable, sequence)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, replicate, none)
import Effect (Effect)
import Effect.Console (log)
import Prelude

-- NOTE - This is all equivalent to Parser.purs but rewriting some functionality to make use of Monads that simplify the code.

{-
"10/2/1962"
 
Regex to parse above date:
\d{1,2}\/\d{1,2}\/\d{4}

We have 3 unique "mini-parsers":
\d{1,2}   -- digits 1 or 2
\/        -- forward slash (escaped since it's a special character)
\d{4}     -- digits 4

In typical Functional fashion, it would be nice to build Parsers for each of these and then combine them somehow to finish the whole job.
In Functional Programming, we combine Functions by composing them where the output of one Function is the input to another.

So if we’re going to combine Parsers to build larger Parsers, each Parser will need to whittle away at our String, doing their part, until we fail or success to parse.

That means that the Parsing State is going to need to be passed from Parser to Parser,
i.e. when the current Parser is done, it passes what’s left of the String to the next Parser who takes a stab at parsing what’s left.

Also, if a single Parser in the chain were to fail, we want to short-circuit the parsing and return the error, hopefully with some useful information as to what went wrong.

In this scenario, we’ll want each Parser to return an Int. But not every Parser is going to parse numbers into an Int.
So, we’ll want our Parser to return the Polymorphic Parameter, a. And there are no constraints on a. a can be any Type.

The other thing we’ll need is the String to parse. We will consume what we need from that String and return what’s left over.
-}

type ParserState a = Tuple String a -- Our polymorphic parameter "a" is second in case we wish to use Functor for Tuple and Functor only Kind * -> *

class ParserError (e :: Type) where
  eof :: e

  invalidChar :: String -> e

type ParseFunction e a = ParserError e => String -> Either e (ParserState a) -- We constrain the ParseFunction to return errors of Type ParserError
{-
When we cannot parse the given String,
we'll return Left e, otherwise
we'll return Right (ParserState a)

e.g. a Parser that parses one of more Numbers given the String:
"123abc"
after calling ParseFunction, we'll have the following return value:
Right (Tuple "abc" 123)
-}

newtype Parser e a = Parser (ParseFunction e a)

{-
A reminder of Functor, Apply and Applicative:

class Functor f where
  map :: ∀ a b. (a -> b) -> f a -> f b

class Functor f <= Apply f where
  apply :: ∀ a b. f (a -> b) -> f a -> f b

infixl 4 apply as <*>

class Apply f <= Applicative f where
  pure :: ∀ a. a -> f a
-}

instance functorParser :: Functor (Parser e) where
  map :: ∀ a b. (a -> b) -> Parser e a -> Parser e b
  -- map fab (Parser fsp) = Parser \s -> map (map fab) (fsp s)      Which equates to
  -- map fab (Parser fsp) = Parser \s -> (<$>) (map fab) (fsp s)    Which equates to
  map fab (Parser fsp) = Parser \s -> map fab <$> (fsp s)

{-
This first version from Parser.purs ia a bit ugly dealing with 2 Lefts the same - so we will improve using Monads
instance applyParser :: Apply (Parser e) where
  apply :: ∀ a b. Parser e (a -> b) -> Parser e a -> Parser e b
  apply (Parser fab) (Parser faa) =
    Parser \s -> case fab s of
        Left err              -> Left err
        Right (Tuple s1 hsp)  -> case faa s1 of
            Left err            -> Left err
            Right (Tuple s2 x)  -> Right $ Tuple s2 (hsp x)
-}

instance applyParser :: Apply (Parser e) where
  apply :: ∀ a b. Parser e (a -> b) -> Parser e a -> Parser e b
  apply p1 p2 = Parser \s -> do -- The first Parser in an Apply returns a Function and the second Parser returns a Value.
    Tuple s1 fab <- parse p1 s
    Tuple s2 a <- parse p2 s1
    pure $ Tuple s2 $ fab a

{-
This new implementation using the power of Monads can be further simplifies with "ap":

import Control.Monad (ap)

instance applyParser :: Apply (Parser e) where
  apply = ap

where "ap" is defined as:

ap :: ∀ a b m. Monad m => m (a -> b) -> m a -> m b
  ap ff fx = do
    f <- ff     -- matches the above: Tuple s1 f <- parse p1 s
    x <- fx     -- matches the above: Tuple s2 f <- parse p2 s1
    pure $ f x
-}    

instance applicativeParser :: Applicative (Parser e) where
  pure :: ∀ a. a -> Parser e a
  pure a = Parser \s -> pure $ (Tuple s a)

{-
where
Parser fab :: Parser e (a -> b)
Parser faa :: Parser e a

faa :: String -> Either e (ParserState a)

fab :: String -> Either e (ParserState (a -> b))
-}

{-
Parser e a = Parser (String -> Either e (Tuple String a))
-}
parse :: ∀ e a. ParserError e => Parser e a -> String -> Either e (ParserState a)
parse (Parser fn) = fn

{-
OR
parse :: ∀ e a. ParserError e => Parser e a -> ParseFunction e a
parse (Parser fn) = fn

but since ParseFunction already has a ParserError contraint, we can omit it here:
parse :: ∀ e a. Parser e a -> ParseFunction e a
parse (Parser fn) = fn
-}

{-
Next we need is a way to parse a single character from the String.
This Parser will be the base Parser that all other Parsers will be built upon.
-}
data MyError = MyEOF | InvalidThing

instance parserErrorMyError :: ParserError MyError where
  eof = MyEOF

  invalidChar _ = InvalidThing

data ParsingError = NoMoreData | MissingStuff String | IntOutOfRange Int

instance parserErrorParsingError :: ParserError ParsingError where
  eof = NoMoreData

  invalidChar = MissingStuff

{-
The approach we chose allows the user of our Parser the flexibility to name their errors whatever they want as we’ve done in these 2 preceding examples.
-}

data PError = EOF | InvalidChar String

derive instance genericPError :: Generic PError _

instance showPError :: Show PError where
  show = genericShow

instance parserErrorPError :: ParserError PError where
  eof = EOF

  invalidChar = InvalidChar

char :: ∀ e. Parser e Char
char = Parser \s -> case uncons s of
  Nothing             -> Left eof
  Just { head, tail } -> Right $ Tuple tail head

twoChars :: ∀ e. Parser e (Tuple Char Char)
twoChars = Tuple <$> char <*> char

{-
As a side note, twoChars explicitly would be:
twoChars :: ∀ e. Parser e (ParseFunction e (Tuple Char Char))

which equates to:
twoChars :: ∀ e. Parser e (ParserError e => String -> Either e (ParserState (Tuple Char Char)))

which equates to:
twoChars :: ∀ e. Parser e (ParserError e => String -> Either e (Tuple String (Tuple Char Char)))
-}

{-
An explanation of twoChars:

1st step:
twoChars :: ∀ e. Parser e (Tuple Char Char)
twoChars = char

Results in a Parser with a single Char (and the remainder of a given String).
We want to return this Char as the first part of a Tuple - so let's map Tuple over the Parser:

2nd step:
twoChars :: ∀ e. Parser e (Tuple Char Char)
twoChars = Tuple <$> char

Now we have our Tuple Data Constructor, which is a Function, Partially Applied in a Context, i.e. inside of Parser.
Luckily for us, Parser is an Applicative, which means it’s also an Apply. So we can use <*> to apply the Function in one Parser to result of another Parser:

3rd step:
twoChars :: ∀ e. Parser e (Tuple Char Char)
twoChars = Tuple <$> char <*> char
-}

{-
<*> does all the heavy lifting for us regarding twoChars (and other Parsers):

apply :: ∀ a b. Parser e (a -> b) -> Parser e a -> Parser e b
apply p1 p2 = Parser \s -> case parse p1 s of
  Left err -> Left err
  Right (Tuple s1 h) -> case parse p2 s1 of
    Left err -> Left err
    Right (Tuple s2 x) -> Right $ Tuple s2 (h x)
-}

-- Monadic/Bind version of twoChars (which we'll call twoCharsB):

twoCharsB :: ∀ e. Parser e (Tuple Char Char)
twoCharsB = do
  c1 <- char
  c2 <- char
  pure $ Tuple c1 c2 -- Don't forget to wrap the Tuple in a Parser since that's the Type we're returning

{-
Alternative version:
twoCharsB :: ∀ e. Parser e (Tuple Char Char)
twoCharsB = char >>= \c1 -> char >>= \c2 -> pure $ Tuple c1 c2
-}  

threeChars :: ∀ e. Parser e (Tuple Char (Tuple Char Char))
threeChars = Tuple <$> char <*> twoChars

-- OR being a tad more explicit:
data Threeple a b c = Threeple a b c

threeChars' :: ∀ e. Parser e (Threeple Char Char Char)
threeChars' = Threeple <$> char <*> char <*> char

-- Another more convenient version of threeChars that produces a String:
threeChars'' :: ∀ e. Parser e String
threeChars'' = fn <$> char <*> char <*> char where
  fn :: Char -> Char -> Char -> String
  fn c1 c2 c3 = fromCharArray [c1, c2, c3]

-- Or
threeChars''' :: ∀ e. Parser e String
threeChars''' = (\c1 c2 c3 -> fromCharArray [c1, c2, c3]) <$> char <*> char <*> char

-- Monadic/Bind version:

threeCharsB :: ∀ e. Parser e String
threeCharsB = do
  c1 <- char
  c2 <- char
  c3 <- char
  pure $ fromCharArray [c1, c2, c3]

-- parse' :: ∀ a. Parser PError a -> ParseFunction PError a
parse' :: ∀ a. Parser PError a -> String -> Either PError (ParserState a)
parse' = parse

{-
Remember the Traverable type class:

class (Functor t, Foldable t) <= Traversable t where
  traverse :: ∀ a b m. Applicative m => (a -> m b) -> t a -> m (t b)
  
  sequence :: ∀ a m. Applicative m => t (m a) -> m (t a)
-}

count :: ∀ e a. Int -> Parser e a -> Parser e (Array a)
count n p
  | n <= 0     = pure []
  | otherwise = sequence (replicate n p)

{-
Make "count" be more general i.e. work on Array, List or any Traversable.

replicate :: ∀ f a. Unfoldable f => Int -> a -> f a
-}

count' :: ∀ e a f. Traversable f => Unfoldable f => Int -> Parser e a -> Parser e (f a)
count' n p
  | n <= 0 = pure none
  | otherwise = sequence (replicate n p)

------------------------------------

-- Making the orignal appliative Parser a Monad:

instance bindParser :: Bind (Parser e) where
  bind :: ∀ a b. Parser e a -> (a -> Parser e b) -> Parser e b
  bind p fap = Parser \s -> do
    Tuple s1 a <- parse p s
    parse (fap a) s1

{-
The equivalent using (Either) >>=

instance bindParser :: Bind (Parser e) where
  bind :: ∀ a b. Parser e a -> (a -> Parser e b) -> Parser e b
  bind p fap = Parser \s -> parse p s >>= \(Tuple s1 a) -> parse (fap a) s1
-}

------------------------------------

-- More parsers now we have the power of Monad.

satisfy :: ∀ e. ParserError e => String -> (Char -> Boolean) -> Parser e Char
satisfy expected pred =
  char >>= \c ->
    if pred c then pure c else Parser \_ -> Left $ invalidChar expected

fail :: ∀ e a. ParserError e => e -> Parser e a
-- fail e = Parser \_ -> Left e -- BUT we can do better
fail e = Parser $ const $ Left e

-- Now we can improve "satisfy":
satisfy' :: ∀ e. ParserError e => String -> (Char -> Boolean) -> Parser e Char
satisfy' expected pred =
  char >>= \c ->
    if pred c then pure c else fail $ invalidChar expected

digit :: ∀ e. ParserError e => Parser e Char
digit = satisfy "digit" (isDecDigit <<< codePointFromChar)

letter :: ∀ e. ParserError e => Parser e Char
letter = satisfy "letter" (isAlpha <<< codePointFromChar)

{-
alphaNum can be defined using the above instead of a brute force approach such as:

alphaNum :: ∀ e. ParserError e => Parser e Char
  alphaNum = Parser \s -> case (parse letter s :: Either PError _) of
    Right x -> Right x
    Left _ -> case parse digit s of
      Left err -> Left err
      Right y -> Right y

Instead we get help with the Alt typeclass (Alt, is a choice we make between 2 Functors):

class Functor f <= Alt f where
  alt :: f a -> f a -> f a

infixl 3 alt as <|>
-}

instance altParser :: Alt (Parser e) where
  alt :: ∀ a. Parser e a -> Parser e a -> Parser e a
  alt p1 p2 = Parser \s -> case parse p1 s of
    Left _ -> parse p2 s
    Right x -> Right x

alphaNum :: ∀ e. ParserError e => Parser e Char
alphaNum = letter <|> digit <|> fail (invalidChar "alphaNum")

count'' :: ∀ e. Int -> Parser e Char -> Parser e String
count'' n p = fromCharArray <$> count n p

------------------------------------
-- Date Parser

newtype Year  = Year Int
newtype Month = Month Int
newtype Day   = Day Int

data DateFormat = YearFirst | MonthFirst

derive instance genericYear :: Generic Year _

derive instance genericMonth :: Generic Month _

derive instance genericDay :: Generic Day _

derive instance genericDateFormat :: Generic DateFormat _

instance showYear :: Show Year where
  show = genericShow

instance showMonth :: Show Month where
  show = genericShow

instance showDay :: Show Day where
  show = genericShow

instance showDateFormat :: Show DateFormat where
  show = genericShow   

type DateParts = {
  year    :: Year,
  month   :: Month,
  day     :: Day,
  format  :: DateFormat
}

optional :: ∀ e a. a -> Parser e a -> Parser e a
optional a p = p <|> pure a

atMost :: ∀ e a. Int -> Parser e a -> Parser e (Array a)
atMost n p
  | n <= 0 = pure []
  | otherwise = optional [] $ p >>= \c -> (c : _) <$> atMost (n - 1) p

atMost' :: ∀ e. Int -> Parser e Char -> Parser e String
atMost' n p = fromCharArray <$> atMost n p

-- Generalise "atMost"

atMostG :: ∀ e a f. Unfoldable f => (a -> f a -> f a) -> Int -> Parser e a -> Parser e (f a)
atMostG cons n p
  | n <= 0 = pure none
  | otherwise = optional none $ p >>= \c -> cons c <$> atMostG cons (n - 1) p

-- and rewriting atMost' to use the generalised version:

atMost'' :: ∀ e. Int -> Parser e Char -> Parser e String
atMost'' n p = fromCharArray <$> atMostG (:) n p

rangeNotGeneralised :: ∀ e a. Int -> Int -> Parser e a -> Parser e (Array a)
rangeNotGeneralised min max p
  | min < 0 || max <= 0 || max < min = pure []
  | otherwise = count min p >>= \cs -> (cs <> _) <$> atMostG (:) (max - min) p

-- And now generalise "range"

range :: ∀ e a f. Semigroup (f a) => Traversable f => Unfoldable f => (a -> f a -> f a) -> Int -> Int -> Parser e a -> Parser e (f a)
range cons min max p
  | min < 0 || max <= 0 || max < min = pure none
  | otherwise = count' min p >>= \cs -> (cs <> _) <$> atMostG cons (max - min) p

-- A specific "range" that gives String that uses the generalised version (above)
range' :: ∀ e. Int -> Int -> Parser e Char -> Parser e String
range' min max p = fromCharArray <$> range (:) min max p

{-
With all the above parsers, we can now handle:
\d{1, 2}    -- range
\d{4}       -- count
-           -- satisfy
-}

constChar :: ∀ e. ParserError e => Char -> Parser e Unit
constChar = void <<< constChar'

constChar' :: ∀ e. ParserError e => Char -> Parser e Char
constChar' c = satisfy (singleton c) (_ == c)

digitsToNum :: String -> Int
digitsToNum = fromMaybe 0 <<< fromString

yearFirst :: ∀ e. ParserError e => Parser e DateParts
yearFirst = do
  year <- Year <<< digitsToNum <$> count'' 4 digit
  constChar '-'
  month <- Month <<< digitsToNum <$> range' 1 2 digit
  constChar '-'
  day <- Day <<< digitsToNum <$> range' 1 2 digit
  pure { year, month, day, format: YearFirst }

monthFirst :: ∀ e. ParserError e => Parser e DateParts
monthFirst = do
  month <- Month <<< digitsToNum <$> range' 1 2 digit
  constChar '/'
  day <- Day <<< digitsToNum <$> range' 1 2 digit
  constChar '/'
  year <- Year <<< digitsToNum <$> count'' 4 digit
  pure { year, month, day, format: MonthFirst }

date :: ∀ e. ParserError e => Parser e DateParts
date = yearFirst <|> monthFirst  

------------------------------------
{-
some and many combinators:

some = 1 or more (+ in Regular Expressions)
many = 0 or more (* in Regular Expressions)

We can define them slightly differently:

some = one and many
many = some or none

The problem here is that they’re defined in terms of each other, i.e. they are Mutually Recursive.
This means that when we call many it will call some which will call many which will call some, etc. until the stack overflows.

In a lazy language like Haskell, this is not a problem.
But PureScript is a strict language, which means it’ll eagerly try to evaluate all Parameter Values to a Function. This causes infinite loops.

E.g.
import Data.Maybe (fromMaybe)
import Partial.Unsafe (unsafeCrashWith)

test :: Int
test = Just 10 # fromMaybe (unsafeCrashWith "Got a Nothing")

Even though the above has a "Just", the default is still eagerly evaluated and crashes your application - To avoid this we need to turn the default into a function:

import Data.Maybe (fromMaybe')                                        -- NOTE the use of fromMaybe'
import Partial.Unsafe (unsafeCrashWith)

test :: Int
test = Just 10 # fromMaybe' \_ -> unsafeCrashWith "Got a Nothing"     -- Passing a Function to fromMaybe' as opposed to a Value like with fromMaybe.

We can say that fromMaybe' is the lazy version of fromMaybe. Let’s compare their Type Signatures:

fromMaybe  :: ∀ a. a -> Maybe a -> a

fromMaybe' :: ∀ a. (Unit -> a) -> Maybe a -> a

There's a useful Lazy typeclass in Control.Lazy:

class Lazy l where
  defer :: (Unit -> l) -> l

If we make our Parser an instance of Lazy, then we’ll be able to use defer in places where calling another Parser would cause an infinite loop.

But first... before we have a lazy Parser, our "some" and "many", which would recurse forever is:
-}

someB4Lazy :: ∀ e a. Parser e a -> Parser e (Array a)
someB4Lazy p = (:) <$> p <*> manyB4Lazy p


manyB4Lazy :: ∀ e a. Parser e a -> Parser e (Array a)
manyB4Lazy p = someB4Lazy p <|> pure []

{-
How does some/many work?

(:) :: a -> Array a -> Array a

p :: Parser e a

(:) <$> p :: Parser e (Array a -> Array a)    -- (:) get’s its first Parameter from p, i.e. a Value of Type a, leaving a Function of Type Array a -> Array a in the resulting Parser.

many p :: Parser e (Array a)

(:) <$> p <*> many p :: Parser e (Array a)    -- (:) <$> p gets its first Parameter from many p, i.e. a Value of Type Array a, leaving a Value of Type Array a in the resulting Parser.


BUT now we need to figure out where to break the loop.
There are only 2 places we could break this loop (we choose one), where we call "many" in "some" or where we call "some" in "many".
-}

instance lazyParser :: Lazy (Parser e a) where
  defer :: (Unit -> Parser e a) -> Parser e a
  defer f = Parser \s -> parse (f unit) s

some :: ∀ e a. Parser e a -> Parser e (Array a)
some p = (:) <$> p <*> defer \_ -> many p

many :: ∀ e a. Parser e a -> Parser e (Array a)
many p = some p <|> pure []

-- Now for a generic "some" and "many"

someG :: ∀ e a f. Unfoldable f => (a -> f a -> f a) -> Parser e a -> Parser e (NonEmpty f a)
someG cons p = (:|) <$> p <*> defer \_ -> manyG cons p

manyG :: ∀ e a f. Unfoldable f => (a -> f a -> f a) -> Parser e a -> Parser e (f a)
manyG cons p = fromNonEmpty cons <$> someG cons p <|> pure none

-- And test against these:

some' :: ∀ e. Parser e Char -> Parser e String
some' p = fromCharArray <<< fromNonEmpty (:) <$> someG (:) p

many' :: ∀ e. Parser e Char -> Parser e String
many' p = fromCharArray <$> manyG (:) p

-- Using "some" and "many"

digits :: ∀ e. ParserError e => Parser e String
digits = some' digit

-- Parser for: (\d{1,4}), ([a-zA-Z ]+)([0-9]*)

ugly :: ∀ e. ParserError e => Parser e (Array String)
ugly = do
  p1 <- range' 1 4 digit
  constChar ','
  constChar ' '
  p2 <- some' (letter <|> constChar' ' ')
  p3 <- many' digit
  pure [p1, p2, p3]

-- The above is monadic - We can also write with applicative style:

uglyA :: ∀ e. ParserError e => Parser e (Array String)
uglyA =
  (\p1 p2 p3 -> [p1, p2, p3]) 
    <$> range' 1 4 digit
    <* constChar ','
    <* constChar ' '
    <*> some' (letter <|> constChar' ' ')
    <*> many' digit

------------------------------------

test :: Effect Unit
test = do
  log $ show $ (parse char "ABC" :: Either PError _)
  log $ show $ (parse twoChars "ABC" :: Either PError _)
  log $ show $ (parse threeChars "ABC" :: Either PError _)
  log "-----------------------------------"
  log $ show $ parse' char "ABC"
  log $ show $ parse' twoChars "ABC"
  log $ show $ parse' threeChars "ABC"
  log "-----------------------------------"
  log $ show $ parse' threeChars'' "A" -- Prints (Left EOF)
  log "-----------------------------------"
  log $ show $ parse' (fromCharArray <$> (count 3 char)) "xyz"
  log "-----------------------------------"
  log $ show $ parse' (count'' 3 digit) "123456"                -- (Right (Tuple "456" "123"))
  log $ show $ parse' (count'' 3 digit) "abc456"                -- (Left (InvalidChar "digit"))
  log $ show $ parse' (count'' 4 letter) "Freddy"               -- (Right (Tuple "dy" "Fred"))
  log $ show $ parse' (count'' 10 alphaNum) "a1b2c3d4e5"        -- (Right (Tuple "" "a1b2c3d4e5"))
  log $ show $ parse' (count'' 10 alphaNum) "######"            -- (Left (InvalidChar "alphaNum"))
  log "-----------------------------------"
  log $ show $ parse' (atMost' (-2) alphaNum) "a1b2c3"          -- (Right (Tuple "a1b2c3" ""))
  log $ show $ parse' (atMost' 2 alphaNum) "$_$"                -- (Right (Tuple "$_$" ""))
  log $ show $ parse' (atMost' 2 alphaNum) "a1b2c3"             -- (Right (Tuple "b2c3" "a1"))
  log "-----------------------------------"
  log $ show $ parse' yearFirst "1999-12-31"                    -- (Right (Tuple "" { day: (Day 31), format: YearFirst, month: (Month 12), year: (Year 1999) }))
  log "-----------------------------------"
  log $ show $ parse' monthFirst "12/31/1999"                   -- (Right (Tuple "" { day: (Day 31), format: MonthFirst, month: (Month 12), year: (Year 1999) }))
  log "-----------------------------------"
  log $ show $ parse' (some' digit) "2343423423abc"             -- (Right (Tuple "abc" "2343423423"))
  log $ show $ parse' (many' digit) "_2343423423abc"            -- (Right (Tuple "_2343423423abc" ""))
  log $ show $ parse' (some' digit) "_2343423423abc"            -- (Left (InvalidChar "digit"))
  log "-----------------------------------"
  log $ show $ parse' ugly "17, some words"                     -- (Right (Tuple "" ["17","some words",""]))
  log $ show $ parse' ugly "5432, some more words1234567890"    -- (Right (Tuple "" ["5432","some more words","1234567890"]))