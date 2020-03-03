module DataTypes where

{-# OPTIONS_GHC -Wall #-}

data Expression
    = Con Int
    | BinOp String Expression Expression
    | Var String
    | UnOp String Expression
    deriving (Show, Eq)

data Law
    = Law String Expression Expression deriving Show

data Step = Step String Expression deriving Show

data PrettyStep = PrettyStep String String deriving Show

data Calculation = Calc Expression [Step] deriving Show

data PrettyCalculation = PrettyCalc Expression [PrettyStep] deriving Show