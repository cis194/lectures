{-# LANGUAGE InstanceSigs #-}

module Lec6 where

import Data.Semigroup
import Test.HUnit
import Test.QuickCheck
import HW4Soln



{- PollEv.com/palmerp063 -}



{-
I wanted to know the syntax of checking any two colors.
I thought I can do something like this:


    Red <> Blue = ...
    a <> b = if a == b then a else ...

    a <> b
      | a == Red && b == Blue = Purple
      | a == b = a
      | otherwise = ...

But it is giving me an error. Here, I am trying to check
if the given two colors are the same, then return that color.

  - https://piazza.com/class/jlfptwuv2ukq5?cid=49
-}



{-
Why is fromInteger showing still as an Integer.
When I run in the terminal I get 5 rather than +|||||

    HW4> fromInteger 5
    5

  - https://piazza.com/class/jlfptwuv2ukq5?cid=52
-}




qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort lessThans ++ [x] ++ qsort greaterThans
  where
    lessThans = filter (<= x) xs
    greaterThans = filter (> x) xs

isOrdered :: Ord a => [a] -> Bool
isOrdered [] = True
isOrdered [x] = True
isOrdered (x : y : ys)
  | x <= y    = isOrdered (y : ys)
  | otherwise = False

prop_qsort_isOrdered :: Ord a => [a] -> Bool
prop_qsort_isOrdered xs =
  isOrdered (qsort xs)


prop_qsort_sameLength :: Ord a => [a] -> Bool
prop_qsort_sameLength xs =
  length xs == length (qsort xs)

quickCheckN n prop = quickCheck (withMaxSuccess n prop)







insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y : ys)
  | x <= y    = x : y : ys
  | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort []       = []
isort (x : xs) = insert x (isort xs)

prop_qsort_isort :: Ord a => [a] -> Bool
prop_qsort_isort xs =
  qsort xs == isort xs

prop_insert_isOrdered :: Ord a => a -> [a] -> Bool
prop_insert_isOrdered x xs =
  not (isOrdered xs) || isOrdered (insert x xs)

prop_insert_isOrdered' :: Ord a => a -> [a] -> Property
prop_insert_isOrdered' x xs =
  isOrdered xs ==> isOrdered (insert x xs)

prop_insert_isOrdered'' :: Ord a => a -> [a] -> Bool
prop_insert_isOrdered'' x xs =
  isOrdered (insert x (qsort xs))



{-
choose :: (System.Random.Random a) => (a, a) -> Gen a
elements :: [a] -> Gen a
oneof :: [Gen a] -> Gen a
frequency :: [(Int, Gen a)] -> Gen a
-}

instance Arbitrary Color where
  arbitrary :: Gen Color
  arbitrary = elements
    [ Red
    , Blue
    , Yellow
    , Purple
    , Orange
    , Green
    , Indeterminate
    ]

{-
Red <> (Yellow <> Orange) == (Red <> Yellow) <> Orange
Red <> Indeterminate == Orange <> Orange
Indeterminate == Orange
-}

prop_color_semigroup_law :: Color -> Color -> Color -> Bool
prop_color_semigroup_law x y z =
  x <> (y <> z) == (x <> y) <> z




data Unit = Unit

constant :: a -> Gen a
constant = return

instance Arbitrary Unit where
  arbitrary :: Gen Unit
  arbitrary = constant Unit



prop_rev_append_194 :: [Int] -> [Int] -> Bool
prop_rev_append_194 xs ys =
  reverse (xs ++ ys) == reverse ys ++ reverse xs
