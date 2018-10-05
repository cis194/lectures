module Lec4 where

import Data.Maybe







{- A play in three acts and five scenes. -}


















{- ACT I, SCENE I : A whole lotta problems -}


{- The problem with strings... -}

-- dayAfter :: String -> String
-- dayAfter "Monday"    = "Tuesday"
-- dayAfter "Tuesday"   = "Friday"
-- dayAfter "Wednesday" = "Thursday"
-- dayAfter "Thursday"  = "Friday"
-- dayAfter "Friday"    = "Saturday"
-- dayAfter "Saturday"  = "Sunday"
-- dayAfter "Sunday"    = "Monday"
-- dayAfter _           = ""







{- The boolean identity crisis -}



{-
QUESTION: What is the type of Java's Collection<E>#add?


Collection<E> (implicit) -> E -> Bool












QUESTION: What do these C functions do?

int control_rod_status = control_rod.rotate(30, true);
int fuel_rod_status = fuel_rod.rotate(0.5, true);
-}









{- The disjoint fields conundrum -}



{-

{ data: Object
, status: string
, error: string
}

QUESTION: What can go wrong?









{ data: Object
, finished: bool
, isLoading: bool
, failed: bool
, error: string
}

QUESTION: What can go wrong?

-}








{- SOLUTION: The problem with strings... -}


data Day
  = Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  deriving Show


dayAfter :: Day -> Day
dayAfter Monday    = Tuesday
dayAfter Tuesday   = Friday
dayAfter Wednesday = Thursday
dayAfter Thursday  = Friday
dayAfter Friday    = Saturday
dayAfter Saturday  = Sunday
dayAfter Sunday    = Monday



{- SOLUTION: The boolean identity crisis -}


{-
QUESTION: What do these C functions do?

int control_rod_status = control_rod.rotate(30, true);
int fuel_rod_status = fuel_rod.rotate(0.5, true);
-}
--
-- controlRodStatus :: Int -> Bool -> Int
-- controlRodStatusCW :: Int -> Int
-- controlRodStatusCCW :: Int -> Int
--
-- data Direction
--   = Clockwise
--   | CounterClockwise
--
-- controlRodStatus' :: Int -> Direction -> Int
--
-- x = controlRodStatus' 30 Clockwise







{- SOLUTION: The disjoint fields conundrum -}


type HttpError = String
data RemoteData error a
  = NotAsked
  | Loading
  | Failure error
  | Success a












{- COMMON THEME: "Making impossible states impossible" -- Richard Feldman
  Conference talk: http://bit.ly/impossible-states -}










{- ACT I, SCENE II : Modeling real world data -}



{-
{ isLoggedIn: bool
, username: string
}
-}


type Username = String
data User
  = Anonymous
  | LoggedIn Username



data SoupFlavor
  = ChickenNoodle
  | Minestrone
  | Pea

data WantsBread
  = WithBread
  | WithoutBread

data SoupOrder
  = Cup SoupFlavor
  | Bowl SoupFlavor WantsBread

data SoupOrder'
  = Cup' SoupFlavor
  | BowlWithBread SoupFlavor
  | BowlWithoutBread SoupFlavor


type Year = Int

data School
  = E
  | W
  | N
  | C

data Student
  = Undergrad School
  | Grad School
  | Alumni Year School










{- ACT I, SCENE III : Built-in types aren't that special -}


data OurBool
  = OurTrue
  | OurFalse

data OurMaybe a
  = OurNothing
  | OurJust a

data OurUnit
  = WhateverWeWant

data OurNever
  = YetAnother OurNever

data OurList a
  = Nil
  | Cons a (OurList a)
  deriving Show

ourListToList :: OurList a -> [a]
ourListToList Nil = []
ourListToList (Cons x xs) = x : ourListToList xs

data Pair a b
  = Pair a b







{- ACT II, SCENE I : Pattern matching, or...
    Help! My data is stuck in an ADT. Bring a ladder, quick! -}


{-

If the type has more than one value constructor, we need to be able to tell
which value constructor was used to create the value.

If the value constructor has data components, we need to be able to
extract those values.

— Real World Haskell on Pattern Matching

-}


data Foo
  = Bar
  | Baz Int
  | Buzz String


toeFoo :: Foo -> Int
toeFoo foo =
  case foo of
    Bar -> 37
    Baz x -> x
    Buzz str -> length str

toeFoo' :: Foo -> Int
toeFoo' Bar = 37
toeFoo' (Baz x) = x
toeFoo' (Buzz str) = length str







badMaybeAdd, goodMaybeAdd, greatMaybeAdd :: Maybe Int -> Maybe Int -> Maybe Int

badMaybeAdd mx my =
  if isJust mx && isJust my then
    Just (fromJust mx + fromJust my)
  else
    Nothing

goodMaybeAdd (Just x) (Just y) = Just $ x + y
goodMaybeAdd (Just x) Nothing  = Nothing
goodMaybeAdd Nothing (Just y)  = Nothing
goodMaybeAdd Nothing Nothing   = Nothing


goodMaybeAdd' (Just x) (Just y) = Just $ x + y
goodMaybeAdd' _        _        = Nothing

greatMaybeAdd mx my = (+) <$> mx <*> my
bestMaybeAdd = liftA2 (+)











goodMap = undefined












badFilter :: (a -> Bool) -> [a] -> [a]
badFilter predicate list =
  if null list then
    []
  else if predicate (head list) then
    head list : badFilter predicate (tail list)
  else
    badFilter predicate (tail list)

goodFilter :: (a -> Bool) -> [a] -> [a]
goodFilter predicate [] = []
goodFilter predicate (x : xs)
  | predicate x = x : goodFilter predicate xs
  | otherwise   = goodFilter predicate xs
