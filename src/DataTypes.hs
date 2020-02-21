module DataTypes where

{-# OPTIONS_GHC -Wall #-}

data Expression
    = Const Float
    | BinOp String Expression Expression
    | Var String
    | UnOp String Expression
    deriving Show

data Law
    = Law String Expression Expression

data Step = Step Law Expression

data Calculation = Calc Expression [Step]