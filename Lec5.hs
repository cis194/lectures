{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Lec5 where

-- TYPECLASSES
















-- WHY DO WE NEED TYPECLASSES



-- contains :: a -> [a] -> Bool
-- contains _ []      = False
-- contains z (h : t) = z == h || contains z t


-- contains :: (a -> a -> Bool) -> a -> [a] -> Bool
-- contains eq _ []      = False
-- contains eq z (h : t) = z `eq` h || contains eq z t


contains :: Eq a => a -> [a] -> Bool
contains _ []      = False
contains z (h : t) = z == h || contains z t

class Equatable a where
  eq :: a -> a -> Bool

instance Equatable Int where
  eq = (==)

neq :: Equatable a => a -> a -> Bool
neq x y = not $ x `eq` y






-- Compare with the Prelude definition of `Eq`















qsort :: Orderable a => [a] -> [a]
qsort []      = []
qsort (h : t) =
  qsort lessThanOrEquals
    ++ [h]
    ++ qsort greaterThans
  where
    lessThanOrEquals = filter (lessThanOrEqualTo h) t
    greaterThans = filter (not . lessThanOrEqualTo h) t




class Orderable a where
  lessThanOrEqualTo :: a -> a -> Bool

instance Orderable Char where
  lessThanOrEqualTo = flip (<=)




-- Compare with the Prelude definition of `Ord`












-- MAKING INSTANCES


data PosInt
  = One
  | OnePlus PosInt


posIntEq :: PosInt -> PosInt -> Bool
posIntEq One One = True
posIntEq (OnePlus n) (OnePlus m) = posIntEq n m
posIntEq _   _   = False



posIntCmp :: PosInt -> PosInt -> Ordering
posIntCmp One One                 = EQ
posIntCmp (OnePlus n) One         = GT
posIntCmp One (OnePlus m)         = LT
posIntCmp (OnePlus n) (OnePlus m) = posIntCmp n m



instance Eq PosInt where
  (==) = posIntEq

instance Ord PosInt where
  compare = posIntCmp





-- SOME MORE COMMON TYPECLASSES

-- Show, Read, Num

posIntToInt :: PosInt -> Int
posIntToInt One = 1
posIntToInt (OnePlus n) = 1 + posIntToInt n

posIntShow :: PosInt -> String
posIntShow = show . posIntToInt

posIntShow' :: PosInt -> String
posIntShow' One = "|"
posIntShow' (OnePlus n) = "|" ++ posIntShow' n

instance Show PosInt where
  show = show . posIntToInt





-- read (show x) == x
-- read . show == id


-- Read is (more or less) the inverse of Show

-- class Addable a where
--   add :: a -> a -> a

-- Num requires a negative representation for our numbers


-- DERIVING

data Day
  = Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  deriving (Eq, Ord, Show, Read)











-- MONOID



class Mergeable a where
  merge :: a -> a -> a


instance Mergeable [a] where
  merge = (++)

instance Mergeable Int where
  merge = (*)

-- 
-- instance Monoid Int where
--   (<>) = (*)
--   mempty = 1

-- Associativity Law
-- merge (merge x y) z == merge x (merge y z)

-- [a]
-- Int
-- (a -> b)



















-- instance Mergeable [a] where




-- instance Mergeable a => Mergeable (Maybe a) where



-- Look at definition of Semigroup, Monoid






-- class Mappable f



class Mappable (f :: * -> *) where
  mapOver :: (a -> b) -> f a -> f b


listMap :: (a -> b) -> [a] -> [b]
listMap f [] = []
listMap f (x : xs) = f x : listMap f xs


instance Mappable [] where
  mapOver = listMap


data Foo a
  = Bar
  | Baz Int a
  | Biz a a String
  | Buz String
  deriving (Show, Eq)



instance Mappable Foo where
  mapOver f Bar = Bar
  mapOver f (Baz n a) = Baz n (f a)
  mapOver f (Biz a1 a2 s) = Biz (f a1) (f a2) s
  mapOver f (Buz s) = Buz s


-- instance Mappable Foo where
