module Presentable where

import DataTypes

--Makes calculation prettier
pretty (Calc expression steps) = PrettyCalc expression [ PrettyStep stepName (present e) | Step stepName e <- steps]

--converts expression to string
present (Con a) = show a
present (Var x) = x
present (UnOp op expr) = presentOp(op) ++ "(" ++ present(expr) ++ ")"
present (BinOp op expr1 expr2) = "(" ++ present(expr1) ++ presentOp(op) ++ present(expr2) ++ ")"

presentOp("Mult") = "*"
presentOp("Sum") = "+"
presentOp("Pow") = "^"
presentOp("Neg") = "-"
presentOp("Div") = "/"
presentOp("Min") = "-"
presentOp(x) = x