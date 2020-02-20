module DataTypes where

{-# OPTIONS_GHC -Wall #-}

data Expression
    = Reference String
    | Application (Expression) [Expression]
    deriving Show

data Law
    = Law String Expression Expression

data Step = Step Law Expression

data Calculation = Calc Expression [Step]