module Lec11 where

import Control.Monad ((>=>), when)
import Data.Array.ST (newArray, runSTUArray, writeArray)
import Data.Array.Unboxed (UArray, (!), bounds)
import Data.Foldable (for_)
import Data.IORef (newIORef, modifyIORef, readIORef, writeIORef)
import Data.Word (Word32)
import System.Environment (getArgs)
import System.IO
  ( IOMode(..)
  , hClose
  , hGetContents
  , hPutStr
  , openFile
  , withFile
  )
import System.IO.Error (catchIOError, isDoesNotExistError, isPermissionError)


{- lec11: IO and ST monads -}

{-
Topics:
  - do notation
  - File I/O
  - Exception handling
  - Mutation with IO
  - ST monad
-}


{- Intro: Cake (recipes)! -}

{-
Read the following lecture notes from a previous edition of CIS 194:
https://www.cis.upenn.edu/~cis194/spring15/lectures/05-IO.html
-}

{- Hello, world! -}

helloWorld :: IO ()
helloWorld = putStrLn "Hello, world!"

helloX :: IO ()
helloX =
  putStrLn "Hello, what is your name?" >>
  getLine >>= (\name ->
  putStrLn $ "Pleased to meet you, " ++ name ++ "!")

{-
do-notation is syntactic sugar for writing monadic expressions.
It is convenient to use for many IO monad computations but
don't forget that at the end of day you are still working with
monads. There is a deterministic algoirthm for converting expressions
written in do-notation to an expression written with `>>=`s.
-}

helloX' :: IO ()
helloX' = do
  putStrLn "Hello, what is your name?"
  name <- getLine
  putStrLn $ "Pleased to meet you, " ++ name ++ "!"


{- Working with files -}



cat :: IO ()
cat =
  getArgs >>= (\args ->
  for_ args (\arg ->
    openFile arg ReadMode >>= (\fileHandle ->
    hGetContents fileHandle >>= (\contents ->
    putStr contents >>
    hClose fileHandle)))
  )

cat' :: IO ()
cat' = do
  args <- getArgs
  for_ args $ \arg -> do
    fileHandle <- openFile arg ReadMode
    contents <- hGetContents fileHandle
    putStr contents
    hClose fileHandle

cat'' :: IO ()
cat'' = getArgs >>= mapM_ (readFile >=> putStr)

reverseLines :: IO ()
reverseLines = do
  args <- getArgs
  case args of
    [filename] -> do
      contents <- readFile filename
      let output = unlines (reverse (lines contents))
      length contents `seq` writeFile filename output
    _ -> putStrLn "usage: ./reverseLines <filename>"

fileStats :: IO ()
fileStats = do
  args <- getArgs
  case args of
    [] -> putStrLn "usage: ./count_words <filename>"
    (filename : _) -> do
      contents <- readFile filename
      let countChars = length contents
      let countWords = length (words contents)
      let countLines = length (lines contents)
      putStrLn ("chars: " ++ show countChars)
      putStrLn ("words: " ++ show countWords)
      putStrLn ("lines: " ++ show countLines)



{- Exception handling -}

sassyCat :: IO ()
sassyCat =
  cat `catchIOError` \e ->
    if isDoesNotExistError e then
      putStrLn "I have no clue what you are talking about"
    else if isPermissionError e then
      putStrLn "Hey! That information is confidential."
    else
      ioError e


{- Mutation with IO -}

simpleMutation :: IO ()
simpleMutation = do
  varA <- newIORef 17
  a0 <- readIORef varA
  writeIORef varA (a0 + 20)
  a1 <- readIORef varA
  print (a0, a1)

{-
Although `count` might look very much like imperative code, it is
important to remember that we are just working with monads here.
`count` can be thought of as a _description_ of how to mutate
variables to compute the number of matching elements in a list.
We say a description because `count` is not actually performing
any mutation. The mutation gets performed by the Haskell runtime
when we use `count` in `main`.
-}

count :: Eq a => a -> [a] -> IO Int
count needle haystack = do
  r <- newIORef 0                 -- r = 0
  for_ haystack $ \elt ->         -- for (elt in haystack)
    when (elt == needle) $ do     --  if (elt == needle)
      cnt <- readIORef r          --    r = r + 1
      writeIORef r (cnt + 1)
  readIORef r                     -- return r

count' :: Eq a => a -> [a] -> Int
count' needle haystack = length (filter (== needle) haystack)

{- Mutation with ST -}

{-
If you analyze `count` very closely you'll realize that although it
depends on mutation, all the mutation is internal to the `count` function.
Within the function we make an IORef and manipulate it and at the end of
the function we ask for the value stored in the IORef, in this case
an Int. Haskell has another monad called ST (which stands for
state threads) that allows us to make use of "local mutation." With the
help of the Haskell type system we can be guaranteed that code that uses
the ST monad is externally pure. That is, to callers, the function
just takes in and returns pure values and the fact that the function
uses mutation is an implementation detail which can be ignored.
-}

count'' :: Eq a => a -> [a] -> Int
count'' needle haystack = runST $ do
  r <- newSTRef 0                 -- r = 0
  for_ haystack $ \elt ->         -- for (elt in haystack)
    when (elt == needle) $ do     --  if (elt == needle)
      cnt <- readSTRef r          --    r = r + 1
      writeSTRef r (cnt + 1)
  readIORef r                     -- return r



{-
Here's a neat trick that you can use for generating unique IDs.
-}

newCounter :: IO (IO Int)
newCounter = do
  r <- newIORef 0
  return $ do
    v <- readIORef r
    writeIORef r (v + 1)
    return v

printCounts :: IO ()
printCounts = do
  c <- newCounter
  print =<< c
  print =<< c
  print =<< c
