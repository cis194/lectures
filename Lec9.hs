{-# LANGUAGE InstanceSigs #-}

module Lec9 where

import Control.Monad (MonadPlus(..), ap, liftM)
import Control.Monad.Fail (MonadFail(..))
import Control.Applicative (Alternative(..))
import Data.Char (isDigit, isSpace)

{- the Parser monad -}

newtype Parser a = Parser (String -> Maybe (a, String))

runParser :: Parser a -> String -> Maybe (a, String)
runParser (Parser k) s = k s


satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s ->
  case s of
    [] -> Nothing
    (c:cs)
      | p c -> Just (c, cs)
      | otherwise -> Nothing


anyChar :: Parser Char
anyChar = satisfy $ \_ -> True

char :: Char -> Parser Char
char c = satisfy $ \c' -> c == c'

space :: Parser Char
space = satisfy isSpace

digit :: Parser Char
digit = satisfy isDigit

anyOf :: [Char] -> Parser Char
anyOf cs = satisfy $ \c -> c `elem` cs

noneOf :: [Char] -> Parser Char
noneOf cs = satisfy $ \c -> not (c `elem` cs)




instance Monad Parser where
  return :: a -> Parser a
  return x = Parser $ \s -> Just (x, s)

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  m >>= k = Parser $ \s ->
    case runParser m s of
      Nothing -> Nothing
      Just (x, s') -> runParser (k x) s'

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure = return
  (<*>) = ap



digitThenChar :: Parser String
digitThenChar =
  digit >>= (\d ->
    anyChar >>= (\c ->
      return [d, c]
    )
  )



string :: String -> Parser String
string "" = return ""
string (c:cs) =
  char c >>= (\first ->
    string cs >>= (\rest ->
      return (first:rest)
    )
  )


eitherOr :: Parser a -> Parser a -> Parser a
eitherOr parser1 parser2 = Parser $ \s ->
  case runParser parser1 s of
    Nothing -> runParser parser2 s
    justSomething -> justSomething

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ \_ -> Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) = eitherOr

choice :: [Parser a] -> Parser a
choice [] = empty
choice (p:ps) = p <|> choice ps

integer :: Parser Int
integer =
  signParser >>= (\sign ->
    oneOrMore digit >>= (\digits ->
      return (read (sign ++ digits))
    )
  )
  where
    signParser = choice [string "+" >> string "", string "-", string ""]

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> return []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p =
  p >>= (\first ->
    zeroOrMore p >>= (\rest ->
      return (first:rest)
    )
  )

csvFile :: Parser [[String]]
csvFile = endBy line eol

line :: Parser [String]
line = sepBy cell (char ',')

cell :: Parser String
cell = quotedCell <|> zeroOrMore (noneOf ",\n\r")

quotedCell :: Parser String
quotedCell =
  char '"' >>
  zeroOrMore quotedChar >>= (\content ->
    char '"' >> return content
  )

quotedChar :: Parser Char
quotedChar = noneOf "\"" <|> (string "\"\"" >> return '"')

eol :: Parser String
eol = choice
  [ string "\n\r"
  , string "\r\n"
  , string "\n"
  , string "\r"
  ] >> return "\n"

endBy :: Parser a -> Parser b -> Parser [a]
endBy p sep = zeroOrMore (p >>= (\x -> sep >> return x))

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = atLeastOne <|> return []
  where
    atLeastOne =
      p >>= (\first ->
        zeroOrMore (sep >> p) >>= (\rest ->
          return (first:rest)
        )
      )

main :: IO ()
main =
  getContents >>= (\c ->
    case runParser csvFile c of
      Nothing -> putStrLn "Error parsing input"
      Just (parsedData, _) -> mapM_ print parsedData
  )



{- Extra stuff we didn't quite cover in class -}



spaces :: Parser ()
spaces = (\_ -> ()) <$> zeroOrMore space

eof :: Parser ()
eof = Parser $ \s -> if null s then Just ((), s) else Nothing
