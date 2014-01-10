module Pyrite.Types where

import Data.List (intercalate)

data Value = Boolean Bool
           | Symbol String
           | String String
           | Number Double
           | List [Value]

instance Show Value where
  show (Boolean True) = "<#t>"
  show (Boolean False) = "<#f>"
  show (Symbol s) = s
  show (String s) = "\"" ++ showWithEscapes s ++ "\""
    where showWithEscapes "" = ""
          showWithEscapes (c:cs) =
            case c == '\\' || c == '"' of
              True -> '\\' : c : showWithEscapes cs
              False -> c : showWithEscapes cs
  show (Number n) = show n
  show (List l) = "(" ++ (intercalate " " $ map show l) ++ ")"

data Error = ParseError String
           | CompileError String
           | GenericError String
           | UnknownError

type ThrowsError = Either Error

instance Show Error where
  show (ParseError msg) = "ParseError: " ++ msg
  show (CompileError msg) = "CompileError: " ++ msg
  show (GenericError msg) = "Error: " ++ msg
  show UnknownError = "Error: an unknown error has occurred"

data Code = Exit
          | Constant Value Code
          | Lookup String Code
          | Test Code Code
          deriving (Show)

data VM = VM { accumulator :: Value
             -- environment :: Env
             , stack       :: [VM]
             , arguments   :: [Value]
             }

instance Show VM where
  show = show . accumulator