{-# LANGUAGE NoMonomorphismRestriction #-}

module Pyrite.Parser where

import Pyrite.Types
import Text.Parsec hiding (parse)
import qualified Text.Parsec (parse)

whitespace = skipMany1 space'
  where space' = space <|> tab <|> newline

symbol = oneOf "~!@#$%^&*-_/=+"

parseBoolean = do
  char '#'
  v <- oneOf "tf"
  notFollowedBy (symbol <|> alphaNum)
  return $ if v == 't' then Boolean True else Boolean False

parseNumber = try parseDouble <|> try parseDecimal <|> try parseInteger
  where parseInteger = do
          n <- many1 digit
          notFollowedBy (symbol <|> alphaNum)
          return $ Number (read n :: Double)
        parseDecimal = do
          char '.'
          n <- many1 digit
          notFollowedBy (symbol <|> alphaNum)
          return $ Number $ (read ("0." ++ n) :: Double)
        parseDouble = do
          n <- many1 digit
          char '.'
          d <- many1 digit
          notFollowedBy (symbol <|> alphaNum)
          return $ Number $ (read (n ++ "." ++ d) :: Double)

parseSymbol = do
  sym <- many1 (digit <|> letter <|> symbol)
  return $ Symbol sym

parseString = do
  char '"'
  s <- many $ escapedChar <|> noneOf "\"\\"
  char '"'
  return $ String s
  where escapedChar = do
          char '\\'
          c <- oneOf "\\\""
          return c

parseList = do
  char '('
  optional whitespace
  contents <- parseValue `sepEndBy` whitespace
  char ')'
  return $ List contents

parseValue = try parseNumber
             <|> try parseBoolean
             <|> try parseSymbol
             <|> try parseString
             <|> parseList

parse :: String -> ThrowsError [Value]
parse src = case Text.Parsec.parse parseValue' "Pyrite" src of
  Left err -> Left $ ParseError (show err)
  Right v -> return v
  where parseValue' = do
          optional whitespace
          parsed <- parseValue `endBy` (whitespace <|> eof)
          optional whitespace
          lookAhead eof
          return parsed
