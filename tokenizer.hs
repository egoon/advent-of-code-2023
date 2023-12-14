module Tokenizer where

import Data.Char (isDigit, digitToInt, isSpace, isAlpha)

data Token = Num Int | Text String | Colon | Pipe | Other Char
    deriving Show

tokens :: String -> [Token]
tokens [] = []
tokens (':':cs) = Colon:tokens cs
tokens ('|':cs) = Pipe:tokens cs
tokens (c:cs)
    | isSpace c = tokens cs
    | isDigit c = tokenNum (digitToInt c) cs 
    | isAlpha c = tokenText (c:[]) cs 
    | otherwise = (Other c):(tokens cs)

tokenNum n [] = [Num n]
tokenNum n (c:cs)
    | isDigit c = tokenNum (n*10 + (digitToInt c)) cs
    | otherwise = (Num n):(tokens (c:cs))

tokenText t [] = [Text (reverse t)]
tokenText t (c:cs)
    | isAlpha c || c == '-' = tokenText (c:t) cs
    | otherwise = (Text (reverse t)):(tokens (c:cs))
