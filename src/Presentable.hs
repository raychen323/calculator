module Presentable where

import DataTypes
solution (Calc e []) = present(e)
solution (Calc e steps) = do Step string expression <- [last steps]
                             present(expression)

finalOutput :: Output -> String
finalOutput (Output start steps) = start ++ concat(steps)

pretty :: Calculation -> Output
pretty (Calc expression steps) = Output (present(expression)) [ "\n  ={" ++ stepName ++ "}\n" ++ present(e) | Step stepName e <- steps]

solution :: Calculation -> String
solution (Calc e []) = present e
solution (Calc e steps) = present expr where
                            Step string expr = last steps

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