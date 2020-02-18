module Data where

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

data Law
    = Law String Expression Expression

data Step = Step LawName Expression

data Calculation = Calc Expression [Step]