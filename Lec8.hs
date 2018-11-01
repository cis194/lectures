{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}

module Lec8 where

import Control.Applicative (Alternative(..), liftA2)
import Data.Char (isDigit, isSpace)
import Data.Semigroup (Semigroup(..))
import Prelude hiding ((<$>), (<*>), (>>=), (>>))




{- Context (a.k.a. the 194 definition of a Monad)-}

class Context (m :: * -> *) where
  inject :: a -> m a
  lift :: (a -> b) -> m a -> m b
  join :: m (m a) -> m a

















{- default instances of standard Haskell typeclasses in terms of Context -}

fmapDefault :: Context m => (a -> b) -> m a -> m b
fmapDefault = lift

pureDefault :: Context m => a -> m a
pureDefault = inject

zapDefault :: Context m => m (a -> b) -> m a -> m b
zapDefault mf mx = join (lift (\f -> lift f mx) mf)

returnDefault :: Context m => a -> m a
returnDefault = inject

bindDefault :: Context m => m a -> (a -> m b) -> m b
bindDefault m k = join (lift k m)

seqDefault :: Context m => m a -> m b -> m b
seqDefault ma mb = ma `bindDefault` (\_ -> mb)


{-
For convenience, I'll also define the standard infix operators for these
functions. These are provided for if you use the built-in Monad typeclass.
-}

infixl 4 <$>, <*>
infixl 1 >>=, >>

(<$>) :: Context m => (a -> b) -> m a -> m b
(<$>) = fmapDefault

(<*>) :: Context m => m (a -> b) -> m a -> m b
(<*>) = zapDefault

(>>=) :: Context m => m a -> (a -> m b) -> m b
(>>=) = bindDefault

(>>) :: Context m => m a -> m b -> m b
(>>)  = seqDefault





{- the Maybe monad -}

{-
data Maybe a
  = Just a
  | Nothing
-}

instance Context Maybe where
  inject :: a -> Maybe a
  inject x = Just x

  lift :: (a -> b) -> Maybe a -> Maybe b
  lift f Nothing = Nothing
  lift f (Just x) = Just (f x)

  join :: Maybe (Maybe a) -> Maybe a
  join Nothing = Nothing
  join (Just Nothing) = Nothing
  join (Just (Just x)) = Just x










{- the Either monad -}


{-
A brief aside on kinds:

    :k Maybe :: * -> *
    :k Either :: * -> * -> *
    :k Either a :: * -> *

You can only make a Monad instance for things of kind * -> *!
-}


{-
data Either a b
  = Left a
  | Right b
-}

instance Context (Either a) where
  inject :: b -> Either a b
  inject x = Right x

  lift :: (b -> c) -> Either a b -> Either a c
  lift f (Left err) = Left err
  lift f (Right x) = Right (f x)

  join :: Either a (Either a b) -> Either a b
  join (Left err) = Left err
  join (Right (Left err)) = Left err
  join (Right (Right x)) = Right x






{- the Reader monad -}


-- NOTE: We skipped this example in lecture due to timing.

newtype Reader env a = Reader (env -> a)

runReader :: Reader env a -> env -> a
runReader (Reader k) env = k env

instance Context (Reader env) where
  inject :: a -> Reader env a
  inject x = Reader $ \env -> x

  lift :: (a -> b) -> Reader env a -> Reader env b
  lift f (Reader k) = Reader $ \env -> f (k env)

  join :: Reader env (Reader env a) -> Reader env a
  join (Reader e2r) = Reader $ \env ->
    let (Reader k) = e2r env in k env

ask :: Reader env env
ask = Reader $ \env -> env










{- the Logger monad -}

newtype Logger w a = Logger (a, [w])

runLogger :: Logger w a -> (a, [w])
runLogger (Logger x) = x

instance Context (Logger w) where
  inject :: a -> Logger w a
  inject x = Logger (x, [])

  lift :: (a -> b) -> Logger w a -> Logger w b
  lift f (Logger (x, ws)) = Logger (f x, ws)

  join :: Logger w (Logger w a) -> Logger w a
  join (Logger (Logger (x, ws'), ws)) = Logger (x, ws ++ ws')

{-
We spent a decent amount of time discussing the implementation of join
with respect to what to return for the list of log values. Some
possibilities were: [], ws, ws', ws ++ ws', ws' ++ ws. The right choice
takes into account that the outer logger runs before the inner one.
-}

record :: w -> Logger w ()
record s = Logger ((), [s])









{-
A quick example to demonstrate how one might use a logger.
-}

recordAndInject :: a -> Logger a a
recordAndInject x = record x >> inject x

factorialsUpTo :: Int -> Logger Int Int
factorialsUpTo 0 = recordAndInject 1
factorialsUpTo n =
  factorialsUpTo (n - 1) >>= (\acc -> recordAndInject (n * acc))
