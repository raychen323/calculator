module DataTypes where

{-# OPTIONS_GHC -Wall #-}

data Expression
    = Con Int
    | BinOp String Expression Expression
    | Var String
    | UnOp String Expression
    deriving (Show, Eq)

data Law
    = Law String Expression Expression

data Step = Step String Expression deriving Show

data Calculation = Calc Expression [Step] deriving Show