module Presentable where

import DataTypes

finalOutput (Output start steps) = start ++ concat(steps)

pretty (Calc expression steps) = Output (present(expression)) [ "\n  ={" ++ stepName ++ "}\n" ++ present(e) | Step stepName e <- steps]

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