 module Lec10 where

import Data.Maybe (fromJust)
import Test.QuickCheck

{- lec10: Purely functional data structures -}



{- Queues -}


type Queue a = ([a], [a])

qempty :: Queue a
qempty = ([], [])

qpush :: a -> Queue a -> Queue a
qpush x (ins, outs) = (x : ins, outs)

qpop :: Queue a -> (Maybe a, Queue a)
qpop ([], [])      = (Nothing, ([], []))
qpop (ins, [])     = qpop ([], reverse ins)
qpop (ins, x:outs) = (Just x, (ins, outs))

qpop' :: Queue a -> Maybe (a, Queue a)
qpop' ([], [])      = Nothing
qpop' (ins, [])     = qpop' ([], reverse ins)
qpop' (ins, x:outs) = Just (x, (ins, outs))

qpeek :: Queue a -> Maybe a
qpeek ([], [])  = Nothing
qpeek (ins, []) = qpeek ([], reverse ins)
qpeek (_, x:_)  = Just x

qlength :: Queue a -> Int
qlength (ins, outs) = length ins + length outs

qtoList :: Queue a -> [a]
qtoList = step . qpop
  where
    step (Nothing, _)   = []
    step (Just x, rest) = x : qtoList rest

qfromList :: [a] -> Queue a
qfromList = foldr qpush qempty





{- Zippers -}



l1 :: [Int]
l1 = [ 1, 2, 3, 5, 6 ]

-- I want to perform the following operations on my list:
--   1. delete 5
--   2. insert 4 after 3
--   3. change 3 to 7

l1' :: [Int]
l1' = [ 1, 2, 7, 4, 6 ]


type Zipper a = ([a], [a])

z1 :: Zipper Int
z1 = ( [ 3, 5, 6 ], [2, 1] )

z2 :: Zipper Int
z2 = ( [ 6 ], [5, 3, 2, 1] )

zfromList :: [a] -> Zipper a
zfromList xs = (xs, [])

ztoList :: Zipper a -> [a]
ztoList (as, bs) = reverse bs ++ as

zmoveForward :: Zipper a -> Zipper a
zmoveForward ([], bs)   = ([], bs)
zmoveForward (a:as, bs) = (as, a:bs)

zmoveBackward :: Zipper a -> Zipper a
zmoveBackward (as, [])   = (as, [])
zmoveBackward (as, b:bs) = (b:as, bs)

zinsert :: a -> Zipper a -> Zipper a
zinsert x (as, bs) = (x:as, bs)

zremoveL :: Zipper a -> Zipper a
zremoveL (as, []) = (as, [])
zremoveL (as, b:bs) = (as, bs)

zremoveR :: Zipper a -> Zipper a
zremoveR ([], bs) = ([], bs)
zremoveR (a:as, bs) = (as, bs)

-- We define the following inline operator to make order of computation clearer
(|>) = flip ($)

updateL1 :: [Int]
updateL1 = l1
  |> zfromList
  |> zmoveForward
  |> zmoveForward
  |> zmoveForward
  |> zremoveR
  |> zinsert 4
  |> zremoveL
  |> zinsert 7
  |> ztoList


append [] l2 = l2
append (x:xs) l2 = x : append xs l2






{- Difference Lists -}

list :: [Int]
list = 1 : 2 : 3 : []

dlist :: DList Int
dlist xs = 1 : 2 : 3 : xs


type DList a = [a] -> [a]


dempty :: DList a
dempty = \xs -> xs

dempty' = id

dsingleton :: a -> DList a
dsingleton x = \xs -> x : xs

dsingleton' x = (x :)

dcons :: a -> DList a -> DList a
dcons x dl = \xs -> x : (dl xs)

dcons' x dl = (x : ) . dl

dappend :: DList a -> DList a -> DList a
dappend dl1 dl2 = \xs -> dl1 (dl2 xs)

dappend' = (.)

dtoList :: DList a -> [a]
dtoList dl = dl []

dtoList' = ($ [])

dfromList :: [a] -> DList a
dfromList [] = dempty
dfromList (x:xs) = x `dcons` dfromList xs

{- DList benchmarks -}

-- run `:set +s` in GHCi to time the evaluation of each expression

benchmark1 :: Char
benchmark1 = last (t 10000 "") where
  t 0 l = l
  t n l = t (n-1) (l ++ "s")

benchmark2 :: Char
benchmark2 = last ((t 10000 dempty) "") where
   t 0 l = l
   t n l = t (n-1) (l `dappend` dsingleton 's')

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving Show

inorderDList :: Tree a -> DList a
inorderDList Empty = dempty
inorderDList (Branch x l r) =
  inorderDList l `dappend` dsingleton x `dappend` inorderDList r
