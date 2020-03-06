module Presentable where

import DataTypes

--Concats type output into a returnable string
finalOutput :: Output -> String
finalOutput (Output start steps) = start ++ concat(steps)

--Makes Calc type into a more readable format
pretty :: Calculation -> Output
pretty (Calc expression steps) = Output (present(expression)) [ "\n  ={" ++ stepName ++ "}\n" ++ present(e) | Step stepName e <- steps]

--converts expression to string
present :: Expression -> String
present (Con a) = show a
present (Var x) = x
present (UnOp op expr) = presentOp(op) ++ "(" ++ present(expr) ++ ")"
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