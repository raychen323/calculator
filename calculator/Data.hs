module Data

{-# OPTIONS_GHC -Wall #-}
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void(Void)
import Data.Functor.Identity (Identity)

type Parser = ParsecT Void String Identity

data Expression
    = Reference String
    | Application (Expression) [Expression]
    deriving Show

data Operation
    = Add Expression Expression
    | Minus Expression Expression
    | Product Expression Expression
    | Division Expression Expression
    | Power Expression Expression
    | Trig Expression Expression

data Trig
    = Sin | Cos | Tan