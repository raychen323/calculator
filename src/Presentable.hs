module Presentable where

import DataTypes

prettyOutput (PrettyCalc start steps) = (Output start [(stepName, expr) | PrettyStep stepName expr <- steps ])

--Makes calculation prettier
pretty :: Calculation -> PrettyCalculation
pretty (Calc expression steps) = PrettyCalc (present expression) [ PrettyStep stepName (present e) | Step stepName e <- steps]

--converts expression to string
present :: Expression -> String
present (Con a) = show a
present (Var x) = x
present (UnOp op expr) = presentOp(op) ++ "(" ++ present(expr) ++ ")"
present (BinOp op expr1 expr2) = "(" ++ present(expr1) ++ presentOp(op) ++ present(expr2) ++ ")"

presentOp :: String -> String
presentOp("Mult") = "*"
presentOp("Sum") = "+"
presentOp("Pow") = "^"
presentOp("Neg") = "-"
presentOp("Div") = "/"
presentOp("Min") = "-"
presentOp(x) = x