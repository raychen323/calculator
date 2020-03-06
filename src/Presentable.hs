module Presentable where

import DataTypes
import Parse
import Simplify
import Laws
import Text.Megaparsec

-- Applies calculation to our parser result
solve :: String -> Calculation
solve eq = calc where
  calc = case (parse parseExpression "" eq) of
        Left _ -> (Calc (Var "error, parsing failed") [])
        Right output -> calculate laws output

--Concats type output into a returnable string
finalOutput :: Output -> String
finalOutput (Output start steps) = start ++ concat(steps)

finalSolution :: Calculation -> String
finalSolution (Calc expr []) = present expr
finalSolution (Calc expr steps) = present answer where
                                            (Step label answer) = (last steps)

--Makes Calc type into a more readable format
pretty :: Calculation -> Output
pretty (Calc expression steps) = Output (present(expression)) [ "\n  ={" ++ stepName ++ "}\n" ++ present(e) | Step stepName e <- steps]

--converts expression to string
present :: Expression -> String
present (Con a) = show a
present (Var x) = x
present (UnOp op expr) = presentOp(op) ++ "(" ++ present(expr) ++ ")"
present (BinOp "derive" expr1 expr2) = "(d/d" ++ present(expr1) ++"(" ++ present(expr2) ++ "))"
present (BinOp op expr1 expr2) = "(" ++ present(expr1) ++ presentOp(op) ++ present(expr2) ++ ")"

--Converts operation strings back to operations
presentOp :: String -> String
presentOp("Mult") = "*"
presentOp("Sum") = "+"
presentOp("Pow") = "^"
presentOp("Neg") = "-"
presentOp("Div") = "/"
presentOp("Min") = "-"
presentOp(x) = x