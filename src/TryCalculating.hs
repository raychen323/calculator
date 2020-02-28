module TryCalculating where
import DataTypes
import Simplify
import Parse

test = calculate [law1] (UnOp "derive" (BinOp "Sum" (Var "a") (Var "b")))

law1 = Law "addition" (UnOp "derive" (BinOp "Sum" (Var "a") (Var "b"))) (BinOp "Sum" (UnOp "derive" (Var "a")) (UnOp "derive" (Var "b")))