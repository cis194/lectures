{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}

module Lec7 where



{-
While the following example might appear contrived at first, consider the
scenario where you are using readMaybe to try to convert Strings to Ints.

    readMaybe :: Read a => String -> Maybe a
-}

maybeSomeArithmetic :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
maybeSomeArithmetic mx my mz =
  case mx of
    Nothing -> Nothing
    Just x ->
      case my of
        Nothing -> Nothing
        Just y ->
          case mz of
            Nothing -> Nothing
            Just z -> Just $ x * (y + z)


{-
Noticing that there is a lot of repeated code above, we can define the
following operator to attempt to clean up the implementation.
-}

infixl 1 >>?
(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
mx >>? f =
  case mx of
    Nothing -> Nothing
    Just x -> f x


maybeSomeArithmetic' :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
maybeSomeArithmetic' mx my mz =
  mx >>? (\x ->
    my >>? (\y ->
      mz >>? (\z ->
        Just $ x * (y + z)
      )
    )
  )

{-
You might think of this as doing the following in an imperative language:

    if mx is Nothing
      Nothing
    else if my is Nothing
      Noting
    else if ...

    else
      x * (y + z)

Note that we are abusing notation and the above is NOT valid Haskell code
-}





type KnightPos = (Int,Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = filter onBoard
    [ (c+2,r-1), (c+2,r+1), (c-2,r-1), (c-2,r+1)
    , (c+1,r-2), (c+1,r+2), (c-1,r-2), (c-1,r+2)
    ]
    where onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8]





withinThreeMoves :: KnightPos -> [KnightPos]
withinThreeMoves pos =
  concat . map moveKnight
    . concat . map moveKnight
    . concat . map moveKnight $ [pos]



{-
As above, there is a lot of repeated code. We can define another operator.
-}


infixr 1 <<~
(<<~) :: (a -> [b]) -> [a] -> [b]
(<<~) f xs = concat (map f xs)

withinThreeMoves' :: KnightPos -> [KnightPos]
withinThreeMoves' pos =
  moveKnight <<~ moveKnight <<~ moveKnight <<~ [pos]

{-
It turns out that the two operators we defined are quite similar.
Let's look at their types:

    (>>?)      :: Maybe a -> (a -> Maybe b) -> Maybe b
    flip (<<~) :: [a]     -> (a -> [b])     -> [b]

The only thing that differs between them is the type constructor.

If we abstract out the type constructor (and add the inject function)...
-}

class Context (m :: * -> *) where
  inject :: a -> m a
  andThen :: m a -> (a -> m b) -> m b

instance Context Maybe where
  inject :: a -> Maybe a
  inject x = Just x
  andThen = (>>?)

instance Context [] where
  inject :: a -> [a]
  inject x = [x]
  andThen = flip (<<~)

{-
The above operators actually give us quite natural Monad instances!
-}
